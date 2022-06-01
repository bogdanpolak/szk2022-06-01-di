unit Features.Training;

interface

uses
  Spring.Collections,
  Vcl.Forms;

type
  IFormFactory = interface(IInvokable)
    ['{42EBD089-861B-4F10-9289-00062A36D289}']
    function MakeTrainingForms(aFormCount: integer): IList<TForm>;
  end;

  IFormStore = interface(IInvokable)
    ['{D46B242C-CD80-4FAA-9324-D724F8B1DD7C}']
    procedure Add(const aForms:TArray<TForm>);
    function GetForms(): IReadOnlyList<TForm>;
  end;

  ITrainingFeature = interface(IInvokable)
    ['{75A0A967-C145-47AB-B9A3-3F389D1EB9AA}']
    procedure GenerateForms(const aCount: Integer);
    procedure ApplyStyle();
    procedure ArrangePosiotion();
  end;

implementation

end.

