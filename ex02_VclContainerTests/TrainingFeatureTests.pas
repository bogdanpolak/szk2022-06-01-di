unit TrainingFeatureTests;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Rtti,
  Delphi.Mocks,
  DUnitX.TestFramework,
  Spring.Collections,
  Vcl.Forms,
  {}
  Features.Training,
  Features.TrainingC;

{$M+}

type

  [TestFixture]
  TTrainingFeatureTests = class
  private
    fRectangleArrangerMock: TMock<IRectangleArranger>;
    fFormFactoryMock: TMock<IFormFactory>;
    fFormStoreMock: TMock<IFormStore>;

    sut: TTrainingFeature;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure Test1;
  end;

implementation

{ TTrainingFeatureTests }

procedure TTrainingFeatureTests.Setup;
begin
  fRectangleArrangerMock := TMock<IRectangleArranger>.Create;
  fFormFactoryMock := TMock<IFormFactory>.Create;
  fFormStoreMock := TMock<IFormStore>.Create;

  sut := TTrainingFeature.Create(fFormFactoryMock, fFormStoreMock,
    fRectangleArrangerMock);
end;

procedure TTrainingFeatureTests.TearDown;
begin
  sut.Free;
end;

procedure TTrainingFeatureTests.Test1;
var
  forms1: IList<TForm>;
begin
  // Arrange
  forms1 := TCollections.CreateObjectList<TForm>
    ([TForm.Create(nil), TForm.Create(nil), TForm.Create(nil)]);
  fFormFactoryMock.Setup
    .WillReturn(TValue.From<IList<TForm>>(forms1))
    .When.MakeTrainingForms(3);
  fFormStoreMock.Setup
    .Expect.Exactly(1)
    .When.Add(It(0).IsAny<TArray<TForm>>);

  // Act
  sut.GenerateForms(3);

  // Asset
  fFormStoreMock.Verify();
  Assert.Pass();
end;

initialization

TDUnitX.RegisterTestFixture(TTrainingFeatureTests);

end.
