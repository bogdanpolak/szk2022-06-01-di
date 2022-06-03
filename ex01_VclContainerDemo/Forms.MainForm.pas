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
    btnArangePosition: TButton;
    btnApplyStyle: TButton;
    GroupBox1: TGroupBox;
    GridPanel1: TGridPanel;
    edtRangeFrom: TEdit;
    edtRangeTo: TEdit;
    procedure btnApplyStyleClick(Sender: TObject);
    procedure btnArangePositionClick(Sender: TObject);
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
  GlobalContainer.RegisterType<IRectangleArranger,TRectangleArranger>();

  // Resolve Composition Root
  GlobalContainer.Build;
  fTrainingFeature := GlobalContainer.Resolve<ITrainingFeature>();
end;

procedure TFormMain.btnCreateFormsClick(Sender: TObject);
begin
  fTrainingFeature.GenerateForms(1 + random(3));
end;

procedure TFormMain.btnArangePositionClick(Sender: TObject);
begin
  fTrainingFeature.ArrangePosiotion();
end;

procedure TFormMain.btnApplyStyleClick(Sender: TObject);
begin
  fTrainingFeature.ApplyStyle();
end;

end.
