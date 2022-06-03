unit TrainingFeatureTests;

interface

uses
  System.SysUtils,
  System.StrUtils,
  Delphi.Mocks,
  DUnitX.TestFramework,
  Spring.Collections,
  Features.TrainingC;

{$M+}

type
  [TestFixture]
  TTrainingFeatureTests = class
  private
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

  // sut := TTrainingFeature.Create();
end;

procedure TTrainingFeatureTests.TearDown;
begin

end;

procedure TTrainingFeatureTests.Test1;
begin
  sut.GenerateForms(3);
end;

initialization

TDUnitX.RegisterTestFixture(TTrainingFeatureTests);

end.
