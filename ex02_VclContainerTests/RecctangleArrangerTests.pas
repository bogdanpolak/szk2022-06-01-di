unit RecctangleArrangerTests;

interface

uses
  DUnitX.TestFramework,
  Spring.Collections;

type
  TRectangle = record
    Left: Integer;
    Top: Integer;
    Rigth: Integer;
    Bottom: Integer;
    IsArranged: Boolean;
  end;

  TRectangleArranger = class
  public
    //    function Arrange(
    //      const aBackground: TRectangle;
    //      const aRectangles: IEnumerable<TRectangle>): IEnumerable<TRectangle>;
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
    [Test]
    procedure When_Given_Then;
  end;

implementation

{ TRectangleArranger }

procedure TRectangleArranger.Arrange(const aBackground: TRectangle;
  var aRectangles: IReadOnlyList<TRectangle>);
begin

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

procedure TRectangleArrangerTests.When_Given_Then;
begin
  // Arrange
  backgrou

  // Act
  sut.Arrange()
  // Assert
end;

initialization

TDUnitX.RegisterTestFixture(TRectangleArrangerTests);

end.
