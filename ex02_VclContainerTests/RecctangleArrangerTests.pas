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

  TRectangleArranger = class
  public
    // function Arrange(
    // const aBackground: TRectangle;
    // const aRectangles: IEnumerable<TRectangle>): IEnumerable<TRectangle>;
    procedure Arrange(
      const aBackground: TRectangle;
      var aRectangles: IReadOnlyList<TRectangle>);
  end;

  [TestFixture]
  TRectangleArrangerTests = class
  private
    sut: TRectangleArranger;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure WhenBackgroundLeftTopIsZero_ThenFirstWasArrenged;
    procedure WhenBackgroundLeftTopIsNonZero_ThenFirstWasArrenged;
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
begin
  aRectangles.First.Left := 10;
  aRectangles.First.Top := 10;
  aRectangles.First.Right := 20;
  aRectangles.First.Bottom := 20;
end;

// -----------------------------------------

procedure TRectangleArrangerTests.Setup;
begin
  sut := TRectangleArranger.Create;
end;

procedure TRectangleArrangerTests.TearDown;
begin
  sut.Free;
end;

procedure TRectangleArrangerTests.WhenBackgroundLeftTopIsZero_ThenFirstWasArrenged;
begin
  // Arrange
  var
  rectagles := TCollections.CreateList<TRectangle>
    ([TRectangle.Create(0, 0, 50, 50)]).AsReadOnlyList();

  // Act
  sut.Arrange(TRectangle.Create(0, 0, 900, 900), rectagles);

  // Assert
  Assert.AreEqual(10, rectagles.First.Left);
  Assert.AreEqual(10, rectagles.First.Top);
  Assert.AreEqual(20, rectagles.First.Right);
  Assert.AreEqual(20, rectagles.First.Bottom);
end;

procedure TRectangleArrangerTests.WhenBackgroundLeftTopIsNonZero_ThenFirstWasArrenged;
begin
  // Arrange
  var
  rectagles := TCollections.CreateList<TRectangle>
    ([TRectangle.Create(0, 0, 50, 50)]).AsReadOnlyList();

  // Act
  sut.Arrange(TRectangle.Create(100, 200, 900, 900), rectagles);

  // Assert
  Assert.AreEqual(110, rectagles.First.Left);
  Assert.AreEqual(210, rectagles.First.Top);
  Assert.AreEqual(120, rectagles.First.Right);
  Assert.AreEqual(220, rectagles.First.Bottom);
end;

initialization

TDUnitX.RegisterTestFixture(TRectangleArrangerTests);

end.
