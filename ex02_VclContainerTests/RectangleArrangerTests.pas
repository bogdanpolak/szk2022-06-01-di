unit RectangleArrangerTests;

interface

uses
  System.SysUtils,
  System.StrUtils,
  DUnitX.TestFramework,
  Spring.Collections;

{$M+}

type
  TRectangle = class
  public
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    IsArranged: Boolean;
    constructor CreateA(
      const aLeft: Integer;
      const aTop: Integer;
      const aWidth: Integer;
      const aHeight: Integer;
      const aIsArranged: Boolean = false);
  end;

  TSettings = class
  public
    MarginLeft: Integer;
    MarginTop: Integer;
    HorizontalSpace: Integer;
  end;

  TRectangleArranger = class
  private
    fSettings: TSettings;
  public
    // function Arrange(
    // const aBackground: TRectangle;
    // const aRectangles: IEnumerable<TRectangle>): IEnumerable<TRectangle>;

    constructor Create(const aSettings: TSettings);
    procedure Arrange(
      const aBackground: TRectangle;
      aRectangles: IReadOnlyList<TRectangle>);
  end;

  [TestFixture]
  TRectangleArrangerTests = class
  private
    sut: TRectangleArranger;
    fSettings: TSettings;
    rectangles: IReadOnlyList<TRectangle>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  private
    procedure GivenManyRectangles_ThenShouldWrap;
    procedure GivenManyRectanglesWithDifferentSizes;
    procedure GivenRectangles_ThenArragedOnesShouldNotBeMoved;
  published
    procedure GivenBackgroundLeftTopAtZero_AndOneRectangle;
    procedure GivenBackgroundLeftTopNonZero_AndOneRectangle;
    procedure GivenTwoRectangles;
  end;

implementation

{ TRectangle }

constructor TRectangle.CreateA(
  const aLeft: Integer;
  const aTop: Integer;
  const aWidth: Integer;
  const aHeight: Integer;
  const aIsArranged: Boolean = false);
begin
  Left := aLeft;
  Top := aTop;
  Width := aWidth;
  Height := aHeight;
  IsArranged := aIsArranged;
end;

{ TRectangleArranger }

constructor TRectangleArranger.Create(const aSettings: TSettings);
begin
  fSettings := aSettings;
end;

procedure TRectangleArranger.Arrange(
  const aBackground: TRectangle;
  aRectangles: IReadOnlyList<TRectangle>);
var
  rectangle: TRectangle;
  x: Integer;
  y: Integer;
begin
  x := aBackground.Left + fSettings.MarginLeft;
  y := aBackground.Top + fSettings.MarginTop;
  for rectangle in aRectangles do
  begin
    rectangle.Left := x;
    rectangle.Top := y;
    x := x + rectangle.Width + fSettings.HorizontalSpace;
  end;
end;

// -----------------------------------------

type
  TRectangleHelper = class helper for TRectangle
    procedure ShouldBe(
      const rectangleName: string;
      const aLeft: Integer;
      const aTop: Integer;
      const aWidth: Integer;
      const aHeight: Integer);
  end;
  { TRectangleHelper }

procedure TRectangleHelper.ShouldBe(
  const rectangleName: string;
  const aLeft, aTop, aWidth, aHeight: Integer);
var
  info: string;
begin
  if (aLeft <> self.Left) or (aTop <> self.Top) or (aWidth <> self.Width) or
    (aHeight <> self.Height) then
  begin
    info := IfThen(rectangleName = '', 'rectangle', rectangleName);
    if (aWidth = self.Width) and (aHeight = self.Height) then
      Assert.Fail(Format('Expected %s position (%d,%d), but found (%d,%d)',
        [info, aLeft, aTop, self.Left, self.Top]))
    else
      Assert.Fail
        (Format('Expected %s (%d,%d) w:%d h:%d, but found (%d,%d) w:%d h:%d',
        [info, aLeft, aTop, aWidth, aHeight, self.Left, self.Top, self.Width,
        self.Height]))
  end;
end;

function GivenRectangles(rects: TArray<TRectangle>): IReadOnlyList<TRectangle>;
begin
  Result := TCollections.CreateObjectList<TRectangle>(rects).AsReadOnlyList();
end;

// -----------------------------------------

procedure TRectangleArrangerTests.Setup;
begin
  fSettings := TSettings.Create;
  with fSettings do
  begin
    MarginLeft := 10;
    MarginTop := 10;
    HorizontalSpace := 10;
  end;

  sut := TRectangleArranger.Create(fSettings);
end;

procedure TRectangleArrangerTests.TearDown;
begin
  sut.Free;
end;

procedure TRectangleArrangerTests.GivenBackgroundLeftTopAtZero_AndOneRectangle;
begin
  // Arrange
  rectangles := GivenRectangles([TRectangle.CreateA(0, 0, 50, 40)]);

  // Act
  sut.Arrange(TRectangle.CreateA(0, 0, 900, 900), rectangles);

  // Assert
  rectangles.First.ShouldBe('rectagles.First', 10, 10, 50, 40);
  Assert.Pass();
end;

procedure TRectangleArrangerTests.GivenBackgroundLeftTopNonZero_AndOneRectangle;
var
  rectangles: IReadOnlyList<TRectangle>;
begin
  // Arrange
  fSettings.MarginLeft := 20;
  fSettings.MarginTop := 30;
  rectangles := GivenRectangles([TRectangle.CreateA(0, 0, 60, 40)]);

  // Act
  sut.Arrange(TRectangle.CreateA(100, 200, 800, 600), rectangles);

  // Assert
  rectangles.First.ShouldBe('rectangles.First', 120, 230, 60, 40);
  Assert.Pass();
end;

procedure TRectangleArrangerTests.GivenTwoRectangles;
begin
  // Arrange
  rectangles := GivenRectangles([TRectangle.CreateA(0, 0, 65, 45),
    TRectangle.CreateA(0, 0, 65, 45)]);

  // Act
  sut.Arrange(TRectangle.CreateA(100, 200, 800, 600), rectangles);

  // Assert
  rectangles.First.ShouldBe('rectangles.First', 110, 210, 65, 45);
  rectangles.Last.ShouldBe('rectangles.Last', 185, 210, 65, 45);
  Assert.Pass();
end;

procedure TRectangleArrangerTests.GivenManyRectangles_ThenShouldWrap;
begin
  // Arrange
  rectangles := GivenRectangles([
    { } TRectangle.CreateA(0, 0, 90, 45),
    { } TRectangle.CreateA(0, 0, 90, 45),
    { } TRectangle.CreateA(0, 0, 90, 45)]);

  // Act
  sut.Arrange(TRectangle.CreateA(0, 0, 250, 600), rectangles);

  // Assert
  rectangles.First.ShouldBe('rectangles.First', 10, 10, 90, 45);
  rectangles[1].ShouldBe('rectangles[1]', 110, 10, 90, 45);
  rectangles[2].ShouldBe('rectangles[2]', 10, 85, 90, 45);
  Assert.Pass();
end;

procedure TRectangleArrangerTests.GivenManyRectanglesWithDifferentSizes;
begin
  // Arrange
  rectangles := GivenRectangles([
    { } TRectangle.CreateA(0, 0, 80, 30),
    { } TRectangle.CreateA(1, 1, 120, 50),
    { } TRectangle.CreateA(2, 2, 100, 40),
    { } TRectangle.CreateA(3, 3, 90, 35)]);

  // Act
  sut.Arrange(TRectangle.CreateA(0, 0, 335, 600), rectangles);

  // Assert
  rectangles.First.ShouldBe('rectangles.First', 10, 10, 80, 30);
  rectangles[1].ShouldBe('rectangles[1]', 100, 10, 120, 50);
  rectangles[2].ShouldBe('rectangles[2]', 230, 10, 100, 40);
  rectangles[3].ShouldBe('rectangles[3]', 10, 60, 90, 35);
  Assert.Pass();
end;

procedure TRectangleArrangerTests.
  GivenRectangles_ThenArragedOnesShouldNotBeMoved;
begin
  // Arrange
  rectangles := GivenRectangles([
    { } TRectangle.CreateA(100, 10, 90, 40, true),
    { } TRectangle.CreateA(1, 1, 90, 40, false),
    { } TRectangle.CreateA(2, 2, 90, 40, false)]);

  // Act
  sut.Arrange(TRectangle.CreateA(0, 0, 335, 600), rectangles);

  // Assert
  rectangles.First.ShouldBe('rectangles.First', 100, 10, 90, 40);
  rectangles[1].ShouldBe('rectangles[1]', 10, 10, 90, 40);
  rectangles[2].ShouldBe('rectangles[2]', 200, 10, 90, 40);
  Assert.Pass();
end;

initialization

TDUnitX.RegisterTestFixture(TRectangleArrangerTests);

end.
