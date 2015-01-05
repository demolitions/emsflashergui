unit messagesWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TmsgForm }

  TmsgForm = class(TForm)
    msgMemo: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure addText(t:AnsiString);
    procedure clear();
  end;

var
  msgForm: TmsgForm;

implementation

{$R *.lfm}

procedure TmsgForm.FormCreate(Sender: TObject);
begin
  msgForm.clear;
end;

procedure TmsgForm.addText(t:AnsiString);
begin
  msgForm.msgMemo.Append('['+DateTimeToStr(Now)+'] '+t);
end;

procedure TmsgForm.clear();
begin
  msgForm.msgMemo.Clear;
end;

end.

