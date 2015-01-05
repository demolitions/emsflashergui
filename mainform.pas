unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  AsyncProcess, FileCtrl, ExtCtrls, Process, messageswindow;

const
  emsRom = 1;
  emsSave = 2;
  emsDump = 3;
  emsFlash = 4;

type

  { TMainForm }

  TMainForm = class(TForm)
    bank2Rom_lbl: TLabel;
    bank1Rom_txt: TEdit;
    bank1Rom_lbl: TLabel;
    bank2Rom_txt: TEdit;
    emsStatus_lbl: TLabel;
    openDlg: TOpenDialog;
    saveDlg: TSaveDialog;
    flashSave1_btn: TButton;
    dumpBank1_btn: TButton;
    dumpSave1_btn: TButton;
    flashBank2_btn: TButton;
    flashSave2_btn: TButton;
    dumpBank2_btn: TButton;
    dumpSave2_btn: TButton;
    scanEMS_btn: TButton;
    flashBank1_btn: TButton;
    emsStatus_shp: TShape;
    Timer1: TTimer;
    asyncproc: TAsyncProcess;
    procedure dumpBank1_btnClick(Sender: TObject);
    procedure dumpBank2_btnClick(Sender: TObject);
    procedure dumpSave1_btnClick(Sender: TObject);
    procedure dumpSave2_btnClick(Sender: TObject);
    procedure flashBank1_btnClick(Sender: TObject);
    procedure flashBank2_btnClick(Sender: TObject);
    procedure flashSave1_btnClick(Sender: TObject);
    procedure flashSave2_btnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure scanEMS_btnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    procedure EMSDetect(fromTimer:boolean);
    procedure EMSDetect();
    procedure EMSScan();
    procedure DumpROMToFile(bank:Integer; filename:String);
    procedure DumpSaveToFile(bank:Integer; filename:String);
    procedure WriteROMFromFile(bank:Integer; filename:String);
    procedure WriteSaveFromFile(bank:Integer; filename:String);
  public
    { public declarations }
    procedure Status(col:TColor; str:String);
    procedure Status(col:TColor);
    procedure Status(str:String);
    procedure SetStatus();
    function AskFileDlg(var filename:String; save:Integer; rom:Integer):boolean;
  end;

var
  Main: TMainForm;
  EMSConnected: boolean = False;
  FormStarted: boolean = False;
  Operating: boolean = False;
  Operation: String = '';
  OperFile: String = '';

implementation

{$R *.lfm}

{ TMainForm }

function FileSizeUnit(filename:String):String;
var
  fs,u:Integer;
  un:String;
begin
  u := 0;
  fs := FileSize(filename);
  while fs > 10240 do begin
    fs := fs div 1024;
    inc(u);
  end;
  if u = 0 then un:='B';
  if u = 1 then un:='KB';
  if u = 2 then un:='MB';
  if u = 3 then un:='GB';
  if u >= 4 then un:='TB';
  Result := IntToStr(fs)+un;
end;

procedure TMainForm.SetStatus();
begin
  if EMSConnected then begin
    { EMS connected }
    if Operating then begin
      { EMS in operation }
      flashBank1_btn.Enabled:=False;
      flashBank2_btn.Enabled:=False;
      flashSave1_btn.Enabled:=False;
      flashSave2_btn.Enabled:=False;
      dumpBank1_btn.Enabled:=False;
      dumpBank2_btn.Enabled:=False;
      dumpSave1_btn.Enabled:=False;
      dumpSave2_btn.Enabled:=False;
      scanEMS_btn.Enabled:=False;
    end else begin
      { EMS idle }
      flashBank1_btn.Enabled:=True;
      flashBank2_btn.Enabled:=True;
      flashSave1_btn.Enabled:=True;
      flashSave2_btn.Enabled:=True;
      dumpBank1_btn.Enabled:=True;
      dumpBank2_btn.Enabled:=True;
      dumpSave1_btn.Enabled:=True;
      dumpSave2_btn.Enabled:=True;
      scanEMS_btn.Enabled:=True;
    end;
  end else begin
    { EMS not connected }
    flashBank1_btn.Enabled:=False;
    flashBank2_btn.Enabled:=False;
    flashSave1_btn.Enabled:=False;
    flashSave2_btn.Enabled:=False;
    dumpBank1_btn.Enabled:=False;
    dumpBank2_btn.Enabled:=False;
    dumpSave1_btn.Enabled:=False;
    dumpSave2_btn.Enabled:=False;
    scanEMS_btn.Enabled:=True;
  end;
end;

procedure TMainForm.Status(col:TColor; str:String);
begin
  Self.Status(col);
  Self.Status(str);
end;

procedure TMainForm.Status(col:TColor);
begin
  Self.emsStatus_shp.Brush.Color := col;
  Self.emsStatus_shp.Repaint;
end;

procedure TMainForm.Status(str:String);
begin
  Self.emsStatus_lbl.Caption := str;
  Operation := str;
end;

procedure TMainForm.EMSDetect(fromTimer:boolean);
var
  AProcess: TProcess;
  AStringList: TStringList;
  x,p: Integer;
  EMSFound: boolean;
  StrStatus: String;
begin
  EMSFound := False;
  if not fromTimer then Self.emsStatus_lbl.Caption:='Scanning EMS';
  AProcess := TProcess.Create(nil);
  AStringList := TStringList.Create;
  AProcess.CommandLine := 'lsusb';
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  AProcess.Execute;
  AStringList.LoadFromStream(AProcess.Output);
  if not fromTimer then msgForm.addText('Process Output:');
  for x := 0 to AStringList.Count -1 do begin
    if not fromTimer then msgForm.addText(AStringList.Strings[x]);
    p := Pos('EMS',AStringList.Strings[x]);
    if p > 0 then EMSFound := True;
  end;
  if EMSFound then begin
    if Operation = 'EMS not connected' then begin
      Self.Status(clGreen,'EMS connected');
    end else begin
      Self.Status(clGreen);
    end;
    if not fromTimer then msgForm.addText('EMS found');
  end else begin
    Self.Status(clRed,'EMS not connected');
    if not fromTimer then msgForm.addText('EMS not found');
  end;
  AStringList.Free;
  AProcess.Free;
  EMSConnected := EMSFound;
  Self.SetStatus;
end;

procedure TMainForm.EMSDetect();
begin
  Main.EMSDetect(False);
end;

procedure TMainForm.EMSScan();
var
  AProcess: TProcess;
  AStringList: TStringList;
  RFlags: TReplaceFlags;
  x,p: Integer;
begin
  if EMSConnected then begin
    msgForm.addText('Scanning EMS..');
    AProcess := TProcess.Create(nil);
    AStringList := TStringList.Create;
    AProcess.CommandLine := 'sudo ems-flasher --title';
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    AProcess.Execute;
    AStringList.LoadFromStream(AProcess.Output);
    for x := 0 to AStringList.Count -1 do begin
      msgForm.addText(AStringList.Strings[x]);
      p := Pos('Bank 0:',AStringList.Strings[x]);
      if p > 0 then begin
        Self.bank1Rom_txt.Text:=StringReplace(AStringList.Strings[x], 'Bank 0: ', '', RFlags);
      end;
      p := Pos('Bank 1:',AStringList.Strings[x]);
      if p > 0 then begin
        Self.bank2Rom_txt.Text:=StringReplace(AStringList.Strings[x], 'Bank 1: ', '', RFlags);
      end;
    end;
    AStringList.Free;
    AProcess.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.CreateForm(TmsgForm, msgForm);
  msgForm.Show;
  EMSDetect;
end;

procedure TMainForm.dumpBank1_btnClick(Sender: TObject);
var
  filename: String;
begin
  if Self.AskFileDlg(filename, emsDump, emsRom) then
  begin
    msgForm.addText('Dumping ROM from Bank 1 to ' + filename);
    Self.DumpROMToFile(1,filename);
  end;
end;

procedure TMainForm.dumpBank2_btnClick(Sender: TObject);
var
  filename: String;
begin
  if Self.AskFileDlg(filename, emsDump, emsRom) then
  begin
    msgForm.addText('Dumping ROM from Bank 2 to ' + filename);
    Self.DumpROMToFile(2,filename);
  end;
end;

procedure TMainForm.dumpSave1_btnClick(Sender: TObject);
var
  filename: String;
begin
  if Self.AskFileDlg(filename, emsDump, emsSave) then
  begin
    msgForm.addText('Dumping Save from Bank 1 to ' + filename);
    Self.DumpSaveToFile(1,filename);
  end;
end;

procedure TMainForm.dumpSave2_btnClick(Sender: TObject);
var
  filename: String;
begin
  if Self.AskFileDlg(filename, emsDump, emsSave) then
  begin
    msgForm.addText('Dumping Save from Bank 2 to ' + filename);
    Self.DumpSaveToFile(2,filename);
  end;
end;

procedure TMainForm.flashBank1_btnClick(Sender: TObject);
var
  filename: String;
begin
  if Self.AskFileDlg(filename, emsFlash, emsRom) then
  begin
    msgForm.addText('Flashing ROM from ' + filename + ' to Bank 1');
    Self.WriteROMFromFile(1,filename);
  end;
end;

procedure TMainForm.flashBank2_btnClick(Sender: TObject);
var
  filename: String;
begin
  if Self.AskFileDlg(filename, emsFlash, emsRom) then
  begin
    msgForm.addText('Flashing ROM from ' + filename + ' to Bank 2');
    Self.WriteROMFromFile(2,filename);
  end;
end;

procedure TMainForm.flashSave1_btnClick(Sender: TObject);
var
  filename: String;
begin
  if Self.AskFileDlg(filename, emsFlash, emsSave) then
  begin
    msgForm.addText('Flashing Save from ' + filename + ' to Bank 1');
    Self.WriteSaveFromFile(1,filename);
  end;
end;

procedure TMainForm.flashSave2_btnClick(Sender: TObject);
var
  filename: String;
begin
  if Self.AskFileDlg(filename, emsFlash, emsSave) then
  begin
    msgForm.addText('Flashing Save from ' + filename + ' to Bank 2');
    Self.WriteSaveFromFile(2,filename);
  end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not FormStarted then begin
    Self.Top:=Main.Top-(msgForm.Height div 2);
    msgForm.Top:=Main.Top+Main.Height+30;
    FormStarted := True;
    Self.Timer1.Enabled:=True;
  end;
end;

procedure TMainForm.scanEMS_btnClick(Sender: TObject);
begin
  msgForm.clear;
  Self.EMSDetect;
  Self.EMSScan;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  if not Operating then begin
    Self.EMSDetect(True);
  end else begin
     if Self.asyncproc.Running then begin
      Self.emsStatus_lbl.Caption:=Operation+':'+FileSizeUnit(OperFile);
      Self.Status(clYellow);
    end else begin
      Self.emsStatus_lbl.Caption:=Operation+' completed';
      Self.asyncproc.Free;
      Operating := False;
      Self.Status(clGreen);
    end;
  end;
  Self.SetStatus();
end;

function TMainForm.AskFileDlg(var filename:String; save:Integer; rom:Integer):boolean;
var
  dlg: TOpenDialog;
  op: String = '';
begin
  Result := False;
  if save = emsDump then begin
    dlg := Self.saveDlg;
    op := 'Dump #PKG# to file';
  end else begin
    dlg := Self.openDlg;
    op := 'Flash #PKG# from file';
  end;
  dlg.Options := dlg.Options + [ofOverwritePrompt];
  dlg.InitialDir := '.';
  if rom = emsRom then begin
    dlg.DefaultExt := 'gb';
    dlg.Filter := 'GameBoy ROM|*.gb|All Files|*';
    dlg.Title := StringReplace(op, '#PKG#', 'ROM', []);
  end else begin
    dlg.DefaultExt := 'sav';
    dlg.Filter := 'GameBoy Save|*.sav|All Files|*';
    dlg.Title := StringReplace(op, '#PKG#', 'Save', []);
  end;
  if dlg.Execute then
  begin
    filename := dlg.filename;
    Result := True;
  end;
end;

procedure TMainForm.DumpROMToFile(bank:Integer; filename:String);
begin
  if not Operating then begin
    Operating := True;
    Operation := 'Dumping ROM';
    OperFile := filename;
    Self.asyncproc:= TAsyncProcess.Create(Self);
    Self.asyncproc.CommandLine:='sudo ems-flasher --bank '+IntToStr(bank)+' --rom --read '+filename;
    Self.asyncproc.Execute;
  end;
end;

procedure TMainForm.DumpSaveToFile(bank:Integer; filename:String);
begin
  if not Operating then begin
    Operating := True;
    Operation := 'Dumping SAVE';
    OperFile := filename;
    Self.asyncproc:= TAsyncProcess.Create(Self);
    Self.asyncproc.CommandLine:='sudo ems-flasher --bank '+IntToStr(bank)+' --save --read '+filename;
    Self.asyncproc.Execute;
  end;
end;

procedure TMainForm.WriteROMFromFile(bank:Integer; filename:String);
begin
  if not Operating then begin
    Operating := True;
    Operation := 'Flashing ROM';
    OperFile := filename;
    Self.asyncproc:= TAsyncProcess.Create(Self);
    Self.asyncproc.CommandLine:='sudo ems-flasher --bank '+IntToStr(bank)+' --rom --write '+filename;
    Self.asyncproc.Execute;
  end;
end;

procedure TMainForm.WriteSaveFromFile(bank:Integer; filename:String);
begin
  if not Operating then begin
   Operating := True;
   Operation := 'Flashing SAVE';
   OperFile := filename;
   Self.asyncproc:= TAsyncProcess.Create(Self);
   Self.asyncproc.CommandLine:='sudo ems-flasher --bank '+IntToStr(bank)+' --save --write '+filename;
   Self.asyncproc.Execute;
  end;
end;

end.

