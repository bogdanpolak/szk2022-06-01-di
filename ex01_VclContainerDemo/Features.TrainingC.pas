unit Features.TrainingC;

interface

uses
  System.SysUtils,
  Spring.Collections,
  Vcl.Forms,
  {}
  Features.Training;

type
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
    fFormFactory: TFormFactory;
    fFormStore: IFormStore;
  public
    constructor Create(
      const aFormFactory: TFormFactory;
      const aFormStore: IFormStore);
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
  const aFormFactory: TFormFactory;
  const aFormStore: IFormStore);
begin
  fFormFactory := aFormFactory;
  fFormStore := aFormStore;
end;

procedure TTrainingFeature.GenerateForms(const aCount: integer);
var
  newForms: IList<TForm>;
begin
  newForms := fFormFactory.MakeTrainingForms(1 + random(3));
  newForms.ForEach(
    procedure(const f: TForm)
    begin
      f.Show;
    end);
  fFormStore.Add(newForms.ToArray());
end;

procedure TTrainingFeature.ApplyStyle;
begin

end;

procedure TTrainingFeature.ArrangePosiotion;
begin

end;

end.

