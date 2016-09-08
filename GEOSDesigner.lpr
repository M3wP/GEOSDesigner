program GEOSDesigner;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, lazcontrols, FormGEOSDesignerMain, DModGEOSDesignerMain,
    FrameGEOSDesignerMain, FormGEOSDesignerPreview, GEOSDesignerCore,
    GEOSGraphics, FormGEOSDesignerNew, FormGEOSDesignerIconEdit, 
FormGEOSDesignerAddElem, FormGEOSDesignerAddGPStrInstr
    { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource:=True;
    Application.Initialize;
    Application.CreateForm(TGEOSDesignerMainForm, GEOSDesignerMainForm);
    Application.CreateForm(TGEOSDesignerMainDMod, GEOSDesignerMainDMod);
    Application.CreateForm(TGEOSDesignerPreviewForm, GEOSDesignerPreviewForm);
    Application.CreateForm(TGEOSDesignerNewForm, GEOSDesignerNewForm);
    Application.CreateForm(TGEOSDesignerIconEditForm, GEOSDesignerIconEditForm);
    Application.CreateForm(TGEOSDesignerAddElemForm, GEOSDesignerAddElemForm);
    Application.CreateForm(TGEOSDesignerAddGPStrInstrForm, 
        GEOSDesignerAddGPStrInstrForm);
    Application.Run;
end.

