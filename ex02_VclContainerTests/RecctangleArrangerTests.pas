unit RecctangleArrangerTests;

interface

uses
  DUnitX.TestFramework,
  Spring.Collections;

type
  TRectangle = class
    Left: Integer;
    Top: Integer;
    Right: Integer;
    Bottom: Integer;
    IsArranged: Boolean;
    constructor Create(
      aLeft: Integer;
      aTop: Integer;
      aRight: Integer;
      aBottom: Integer;
      aIsArranged: Boolean = false);
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

constructor TRectangle.Create(
  aLeft, aTop, aRight, aBottom: Integer;
  aIsArranged: Boolean = false);
begin
  Left := aLeft;
  Top := aTop;
  Right := aRight;
  Bottom := aBottom;
  IsArranged := aIsArranged;
end;

{ TRectangleArranger }

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
    rectangle.Right := x + rectangle.Right;
    rectangle.Bottom := y + rectangle.Bottom;
    x := x + rectangle.Right - rectangle.Left + fSettings.HorizontalSpace;
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
    ([TRectangle.Create(0, 0, 50, 50)]).AsReadOnlyList();

  // Act
  sut.Arrange(TRectangle.Create(0, 0, 900, 900), rectagles);

  // Assert
  Assert.AreEqual(fSettings.MarginLeft, rectagles.First.Left);
  Assert.AreEqual(fSettings.MarginTop, rectagles.First.Top);
  Assert.AreEqual(fSettings.MarginLeft + 50, rectagles.First.Right);
  Assert.AreEqual(fSettings.MarginTop + 50, rectagles.First.Bottom);
end;

procedure TRectangleArrangerTests.
  GivenBackgroundLeftTopIsNonZero_ThenFirstWasArrenged;
begin
  // Arrange
  var
  rectagles := TCollections.CreateList<TRectangle>
    ([TRectangle.Create(0, 0, 60, 40)]).AsReadOnlyList();

  // Act
  sut.Arrange(TRectangle.Create(100, 200, 900, 900), rectagles);

  // Assert
  Assert.AreEqual(fSettings.MarginLeft + 100, rectagles.First.Left);
  Assert.AreEqual(fSettings.MarginTop + 200, rectagles.First.Top);
  Assert.AreEqual(fSettings.MarginLeft + 100 + 60, rectagles.First.Right);
  Assert.AreEqual(fSettings.MarginTop + 200 + 40, rectagles.First.Bottom);
end;

procedure TRectangleArrangerTests.
  GivenHavingTwoRectangles_ThenFirstAndLastWasArrenged;
begin
  // Arrange
  var
  rectagles := TCollections.CreateList<TRectangle>
    ([TRectangle.Create(0, 0, 60, 40), TRectangle.Create(0, 0, 60, 40)])
    .AsReadOnlyList();

  // Act
  sut.Arrange(TRectangle.Create(100, 200, 900, 900), rectagles);

  // Assert
  Assert.AreEqual(fSettings.MarginLeft + 100, rectagles.First.Left);
  Assert.AreEqual(fSettings.MarginTop + 200, rectagles.First.Top);
  Assert.AreEqual(fSettings.MarginLeft + 100 + 60, rectagles.First.Right);
  Assert.AreEqual(fSettings.MarginTop + 200 + 40, rectagles.First.Bottom);

  Assert.AreEqual(fSettings.MarginLeft + 100 + 60 + 10, rectagles.Last.Left);
  Assert.AreEqual(fSettings.MarginTop + 200, rectagles.Last.Top);
  Assert.AreEqual(fSettings.MarginLeft + 100 + 60 + 60 + 10, rectagles.Last.Right);
  Assert.AreEqual(fSettings.MarginTop + 200 + 40, rectagles.Last.Bottom);
end;

constructor TRectangleArranger.Create(const aSettings: TSettings);
begin
  fSettings := aSettings;
end;

initialization

TDUnitX.RegisterTestFixture(TRectangleArrangerTests);

end.
