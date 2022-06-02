unit RectangleArrangerTests;

interface

uses
  DUnitX.TestFramework,
  Spring.Collections;

type
  TRectangle = class
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
      var aRectangles: IReadOnlyList<TRectangle>);
  end;

  [TestFixture]
  TRectangleArrangerTests = class
  private
    sut: TRectangleArranger;
    fSettings: TSettings;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure GivenBackgroundLeftTopIsZero_ThenFirstWasArrenged;
    procedure GivenBackgroundLeftTopIsNonZero_ThenFirstWasArrenged;
    procedure GivenHavingTwoRectangles_ThenFirstAndLastWasArrenged;
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
  var aRectangles: IReadOnlyList<TRectangle>);
var
  rectangle: TRectangle;
begin
  var x := aBackground.Left + fSettings.MarginLeft;
  var y := aBackground.Top + fSettings.MarginTop;

  for rectangle in aRectangles do
  begin
    rectangle.Left := x;
    rectangle.Top := y;
    x := x + rectangle.Width - rectangle.Left + fSettings.HorizontalSpace;
  end;
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

procedure TRectangleArrangerTests.
  GivenBackgroundLeftTopIsZero_ThenFirstWasArrenged;
begin
  // Arrange
  var
  rectagles := TCollections.CreateList<TRectangle>
    ([TRectangle.CreateA(0, 0, 50, 40)]).AsReadOnlyList();

  // Act
  sut.Arrange(TRectangle.CreateA(0, 0, 900, 900), rectagles);

  // Assert
  Assert.AreEqual(fSettings.MarginLeft, rectagles.First.Left);
  Assert.AreEqual(fSettings.MarginTop, rectagles.First.Top);
  Assert.AreEqual(50, rectagles.First.Width);
  Assert.AreEqual(40, rectagles.First.Height);
end;

procedure TRectangleArrangerTests.
  GivenBackgroundLeftTopIsNonZero_ThenFirstWasArrenged;
var
  rectangles: IReadOnlyList<TRectangle>;
begin
  // Arrange
  rectangles := TCollections.CreateObjectList<TRectangle>
    ([TRectangle.CreateA(0, 0, 60, 40)]).AsReadOnlyList;

  // Act
  sut.Arrange(TRectangle.CreateA(100, 200, 800, 600), rectangles);

  // Assert
  Assert.AreEqual(fSettings.MarginLeft + 100, rectangles.First.Left);
  Assert.AreEqual(fSettings.MarginTop + 200, rectangles.First.Top);
  Assert.AreEqual(60, rectangles.First.Width);
  Assert.AreEqual(40, rectangles.First.Height);
end;

procedure TRectangleArrangerTests.
  GivenHavingTwoRectangles_ThenFirstAndLastWasArrenged;
begin
  // Arrange
  var
  rectagles := TCollections.CreateList<TRectangle>
    ([TRectangle.CreateA(0, 0, 65, 45), TRectangle.CreateA(0, 0, 65, 45)])
    .AsReadOnlyList();

  // Act
  sut.Arrange(TRectangle.CreateA(100, 200, 800, 600), rectagles);

  // Assert
  Assert.AreEqual(fSettings.MarginLeft + 100, rectagles.First.Left);
  Assert.AreEqual(fSettings.MarginTop + 200, rectagles.First.Top);

  Assert.AreEqual(fSettings.MarginLeft + 100 + 60 + 10, rectagles.Last.Left);
  Assert.AreEqual(fSettings.MarginTop + 200, rectagles.Last.Top);
end;

initialization

TDUnitX.RegisterTestFixture(TRectangleArrangerTests);

end.
