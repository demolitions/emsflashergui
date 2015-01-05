program emsflashergui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainform
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='EMS Flasher GUI';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, main);
  Application.Run;
end.

