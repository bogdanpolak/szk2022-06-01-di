unit TrainingFeatureTests;

interface

uses
  System.SysUtils,
  System.StrUtils,
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
  Assert.Fail;
end;

procedure TTrainingFeatureTests.TearDown;
begin

end;

procedure TTrainingFeatureTests.Test1;
begin

end;

initialization

TDUnitX.RegisterTestFixture(TTrainingFeatureTests);

end.
