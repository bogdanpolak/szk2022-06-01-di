program ex02_VclContainerTests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  {$ENDIF }
  DUnitX.TestFramework,
  RectangleArrangerTests in 'RectangleArrangerTests.pas',
  Features.Training in '..\ex01_VclContainerDemo\Features.Training.pas',
  Features.TrainingC in '..\ex01_VclContainerDemo\Features.TrainingC.pas',
  Forms.TrainingForm in '..\ex01_VclContainerDemo\Forms.TrainingForm.pas' {TrainingForm},
  TrainingFeatureTests in 'TrainingFeatureTests.pas',
  Delphi.Mocks.AutoMock in 'DelphiMocks\Delphi.Mocks.AutoMock.pas',
  Delphi.Mocks.Behavior in 'DelphiMocks\Delphi.Mocks.Behavior.pas',
  Delphi.Mocks.Expectation in 'DelphiMocks\Delphi.Mocks.Expectation.pas',
  Delphi.Mocks.Helpers in 'DelphiMocks\Delphi.Mocks.Helpers.pas',
  Delphi.Mocks.Interfaces in 'DelphiMocks\Delphi.Mocks.Interfaces.pas',
  Delphi.Mocks.MethodData in 'DelphiMocks\Delphi.Mocks.MethodData.pas',
  Delphi.Mocks.ObjectProxy in 'DelphiMocks\Delphi.Mocks.ObjectProxy.pas',
  Delphi.Mocks.ParamMatcher in 'DelphiMocks\Delphi.Mocks.ParamMatcher.pas',
  Delphi.Mocks in 'DelphiMocks\Delphi.Mocks.pas',
  Delphi.Mocks.Proxy in 'DelphiMocks\Delphi.Mocks.Proxy.pas',
  Delphi.Mocks.Proxy.TypeInfo in 'DelphiMocks\Delphi.Mocks.Proxy.TypeInfo.pas',
  Delphi.Mocks.ReturnTypePatch in 'DelphiMocks\Delphi.Mocks.ReturnTypePatch.pas',
  Delphi.Mocks.Utils in 'DelphiMocks\Delphi.Mocks.Utils.pas',
  Delphi.Mocks.Validation in 'DelphiMocks\Delphi.Mocks.Validation.pas',
  Delphi.Mocks.WeakReference in 'DelphiMocks\Delphi.Mocks.WeakReference.pas',
  Delphi.Mocks.When in 'DelphiMocks\Delphi.Mocks.When.pas';

{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger : ITestLogger;
{$ENDIF}
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //When true, Assertions must be made during tests;
    runner.FailsOnNoAsserts := False;

    //tell the runner how we will log things
    //Log to the console window if desired
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.
