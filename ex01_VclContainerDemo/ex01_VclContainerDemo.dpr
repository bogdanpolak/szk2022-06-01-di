program ex01_VclContainerDemo;

uses
  Vcl.Forms,
  Forms.MainForm in 'Forms.MainForm.pas' {FormMain},
  Forms.TrainingForm in 'Forms.TrainingForm.pas' {TrainingForm},
  Features.Training in 'Features.Training.pas',
  Features.TrainingC in 'Features.TrainingC.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
