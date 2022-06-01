unit Forms.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Features.Training,
  Features.TrainingC,
  Spring.Container,
  Spring.Collections;

type
  TFormMain = class(TForm)
    btnCreateForms: TButton;
    Button2: TButton;
    Button3: TButton;
    GroupBox1: TGroupBox;
    GridPanel1: TGridPanel;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnCreateFormsClick(Sender: TObject);
  private
    fTrainingFeature: ITrainingFeature;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  // Register
  GlobalContainer.RegisterType<IFormFactory, TFormFactory>();
  GlobalContainer.RegisterType<IFormStore, TMemoryFormStore>();
  GlobalContainer.RegisterType<ITrainingFeature, TTrainingFeature>();

  // Resolve Composition Root
  GlobalContainer.Build;
  fTrainingFeature := GlobalContainer.Resolve<ITrainingFeature>();
end;

procedure TFormMain.btnCreateFormsClick(Sender: TObject);
begin
  fTrainingFeature..MakeTrainingForms(1 + random(3));
  Forms.ForEach(
    procedure(const Form: TTrainingForm)
    begin
      Form.Show;
    end);
  fForms.AddRange(Forms);
end;

end.
