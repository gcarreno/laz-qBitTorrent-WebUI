unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  PairSplitter, ExtCtrls, StdCtrls, Menus, ActnList, StdActns, qBitTorrentWebUI;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actFileSetActive: TAction;
    actTestGetTorrents: TAction;
    actTestExecShutdown: TAction;
    alMain: TActionList;
    actFileExit: TFileExit;
    btnFileExit: TButton;
    btnTestExecShutdown: TButton;
    btnTestGetTorrents: TButton;
    chkFileActive: TCheckBox;
    divbGetMethhods: TDividerBevel;
    divbCommands: TDividerBevel;
    mnuTestGetTorrents: TMenuItem;
    mnuTestExecShutdown: TMenuItem;
    mnuSep2: TMenuItem;
    mnuTest: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuFile: TMenuItem;
    mmMain: TMainMenu;
    memInfo: TMemo;
    memLog: TMemo;
    psMain: TPairSplitter;
    pssInfo: TPairSplitterSide;
    pssLog: TPairSplitterSide;
    panButtons: TPanel;
    qbttMain: TqBitTorrentWebUI;
    stLabelLog: TStaticText;
    stLabelInfo: TStaticText;
    procedure actFileSetActiveExecute(Sender: TObject);
    procedure actTestExecShutdownExecute(Sender: TObject);
    procedure actTestGetTorrentsExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure Log(const aMsg: String);
    procedure Info(const aMsg: String);
  end;

var
  frmMain: TfrmMain;

implementation

uses
   LCLType;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Log('Starting...');
{$IFDEF LINUX}
  actFileExit.ShortCut := KeyToShortCut(VK_Q, [ssCtrl]);
{$ENDIF}
{$IFDEF WINDOWS}
  actFileExit.ShortCut := KeyToShortCut(VK_X, [ssAlt]);
{$ENDIF}
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := True;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TfrmMain.Log(const aMsg: String);
begin
  memLog.Lines.Add(aMsg);
  Application.ProcessMessages;
end;

procedure TfrmMain.Info(const aMsg: String);
begin
  memInfo.Lines.Add(aMsg);
  Application.ProcessMessages;
end;

procedure TfrmMain.actFileSetActiveExecute(Sender: TObject);
begin
  if chkFileActive.Checked then
    Log('Active: True')
  else
    Log('Active: False');
  qbttMain.Active := chkFileActive.Checked;
  Info('API Version: ' + IntToStr(qbttMain.APIVersion));
  Info('Min API Version: ' + IntToStr(qbttMain.MinAPIVersion));
  Info('qBitTorrent Version: ' + qbttMain.qBitTorrentVersion);
end;

procedure TfrmMain.actTestExecShutdownExecute(Sender: TObject);
var
  bShutdownResult: Boolean;
begin
  Log('Shutting client down.');
  try
    bShutdownResult := qbttMain.ExecShutdown;
    // If we don't do this it will trigger actFileSetActive
    chkFileActive.Action := nil;
    chkFileActive.Checked := False;
    chkFileActive.Action := actFileSetActive;
    if bShutdownResult then
    begin
      Log(#9'Success.');
    end
    else
    begin
      Log(#9'Failed.');
    end;
  except
    on E:Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TfrmMain.actTestGetTorrentsExecute(Sender: TObject);
var
  bGetTorrentsResult: Boolean;
begin
  Log('Asking for torrents.');
  try
    bGetTorrentsResult := qbttMain.GetTorrents;
    if bGetTorrentsResult then
    begin
      Log(#9'Success.');
    end
    else
    begin
      Log(#9'Failed.');
    end;
  except
    on E:Exception do
      Log('Error: ' + E.Message);
  end;
end;

end.

