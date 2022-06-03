unit Features.TrainingC;

interface

uses
  System.Types,
  System.SysUtils,
  System.UITypes,
  Spring,
  Spring.Collections,
  Spring.Container.Common,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  {}
  Features.Training;

type
  TRectangleArranger = class(TInterfacedObject, IRectangleArranger)
  public
    procedure Arrange(const aFormList: IReadOnlyList<TForm>);
  private
    function NoFormAtPos(
      const formList: IReadOnlyList<TForm>;
      const p: TPoint): boolean;
  end;

  TFormFactory = class(TInterfacedObject, IFormFactory)
  private
  public
    function MakeTrainingForms(aFormCount: integer): IList<TForm>;
  end;

  TMemoryFormStore = class(TInterfacedObject, IFormStore)
  private
    fForms: IList<TForm>;
  public
    constructor Create();
    procedure Add(const aForm: TForm); overload;
    procedure Add(const aForms: TArray<TForm>); overload;
    function GetForms(): IReadOnlyList<TForm>;
  end;

  TTrainingFeature = class(TInterfacedObject, ITrainingFeature)
  private
    fFormFactory: IFormFactory;
    fFormStore: IFormStore;
    fRectangleArranger: IRectangleArranger;
  public
    [Inject]
    constructor Create(
      const aFormFactory: IFormFactory;
      const aFormStore: IFormStore;
      const aRectangleArranger: IRectangleArranger);
    procedure GenerateForms(const aCount: integer);
    procedure ApplyStyle();
    procedure ArrangePosiotion();
  end;

implementation

uses
  Forms.TrainingForm;

{ TTrainingFormFactory }

function TFormFactory.MakeTrainingForms(aFormCount: integer): IList<TForm>;
var
  idx: integer;
begin
  Result := TCollections.CreateList<TForm>;
  for idx := 0 to aFormCount - 1 do
    Result.Add(TTrainingForm.Create(nil));
end;

{ TMemoryFormStore }

constructor TMemoryFormStore.Create;
begin
  fForms := TCollections.CreateObjectList<TForm>();
end;

procedure TMemoryFormStore.Add(const aForm: TForm);
begin
  fForms.Add(aForm);
end;

procedure TMemoryFormStore.Add(const aForms: TArray<TForm>);
begin
  fForms.AddRange(aForms);
end;

function TMemoryFormStore.GetForms: IReadOnlyList<TForm>;
begin
  Result := fForms.AsReadOnlyList();
end;

{ TTrainingFeature }

constructor TTrainingFeature.Create(
  const aFormFactory: IFormFactory;
  const aFormStore: IFormStore;
  const aRectangleArranger: IRectangleArranger);
begin
  Guard.CheckNotNull(aFormFactory, 'aFormFactory');
  Guard.CheckNotNull(aFormStore, 'aFormStore');
  Guard.CheckNotNull(aRectangleArranger, 'aRectangleArranger');
  fFormFactory := aFormFactory;
  fFormStore := aFormStore;
  fRectangleArranger := aRectangleArranger;
end;

procedure TTrainingFeature.GenerateForms(const aCount: integer);
var
  newForms: IList<TForm>;
begin
  newForms := fFormFactory.MakeTrainingForms(aCount);
  newForms.ForEach(
    procedure(const f: TForm)
    begin
      f.Show;
    end);
  fFormStore.Add(newForms.ToArray());
end;

procedure TTrainingFeature.ApplyStyle;
const
  colors: TArray<TColor> = [TColors.Aliceblue, TColors.Aqua, TColors.Aquamarine,
    TColors.Azure, TColors.Beige, TColors.Bisque, TColors.Blanchedalmond,
    TColors.Blueviolet, TColors.Burlywood, TColors.Cadetblue,
    TColors.Chartreuse, TColors.Chocolate, TColors.Coral,
    TColors.Cornflowerblue, TColors.Cornsilk, TColors.Crimson, TColors.Deeppink,
    TColors.Deepskyblue, TColors.Dodgerblue, TColors.Firebrick,
    TColors.Floralwhite, TColors.Forestgreen, TColors.Gainsboro, TColors.Gold,
    TColors.Goldenrod, TColors.Greenyellow, TColors.Honeydew, TColors.Hotpink];
  colorNames: TArray<string> = ['Aliceblue', 'Aqua', 'Aquamarine', 'Azure',
    'Beige', 'Bisque', 'Blanchedalmond', 'Blueviolet', 'Burlywood', 'Cadetblue',
    'Chartreuse', 'Chocolate', 'Coral', 'Cornflowerblue', 'Cornsilk', 'Crimson',
    'Deeppink', 'Deepskyblue', 'Dodgerblue', 'Firebrick', 'Floralwhite',
    'Forestgreen', 'Gainsboro', 'Gold', 'Goldenrod', 'Greenyellow', 'Honeydew',
    'Hotpink'];
var
  form: TForm;
  colorIdx: integer;
  ctrl: TControl;
  point: TPoint;
begin
  for form in fFormStore.GetForms() do
  begin
    colorIdx := random(Length(colors));
    form.Color := colors[colorIdx];
    point := TPoint.Create(20, 20);
    ctrl := form.ControlAtPos(point, true, true, true);
    if ctrl = nil then
      Exit;
    (ctrl as TLabel).Caption := colorNames[colorIdx];
  end;
end;

procedure TTrainingFeature.ArrangePosiotion;
begin
  fRectangleArranger.Arrange(fFormStore.GetForms());
end;

{ TRectangleArranger }

function TRectangleArranger.NoFormAtPos(
  const formList: IReadOnlyList<TForm>;
  const p: TPoint): boolean;
var
  form: TForm;
begin
  for form in formList do
  begin
    if (form.Left <= p.X) and (p.X < form.Left + form.Width) and
      (form.Top <= p.Y) and (p.Y < form.Top + form.Height) and (form.Tag > 0)
    then
      Exit(false);
  end;
  Result := true;
end;

procedure TRectangleArranger.Arrange(const aFormList: IReadOnlyList<TForm>);
const
  FormHeight = 150;
  FormWidth = 300;
var
  start: TPoint;
  availableSlots: IList<TPoint>;
  X: integer;
  Y: integer;
  form: TForm;
begin
  start := TPoint.Create(10, 10);
  availableSlots := TCollections.CreateList<TPoint>();
  X := start.X;
  Y := start.Y;
  while (Y + FormHeight < Screen.DesktopHeight) do
  begin
    if NoFormAtPos(aFormList, TPoint.Create(X + 10, Y + 10)) then
      availableSlots.Add(TPoint.Create(X, Y));
    X := X + FormWidth;
    if (X + FormWidth >= Screen.DesktopWidth) then
    begin
      X := start.X;
      Y := Y + FormHeight;
    end;
  end;
  for form in aFormList do
    if (form.Tag = 0) and (availableSlots.Count > 0) then
    begin
      form.Left := availableSlots.First.X;
      form.Top := availableSlots.First.Y;
      form.Width := FormWidth;
      form.Height := FormHeight;
      form.Tag := 1;
      availableSlots.Delete(0);
    end;

  // 320 x 150
end;

end.
