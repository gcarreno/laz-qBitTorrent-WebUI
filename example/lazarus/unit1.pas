unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  PairSplitter, ExtCtrls, StdCtrls, Menus, ActnList, StdActns, qBitTorrentWebUI;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actTestGetqBitTorrentVersion: TAction;
    actTestGetMinApiVersion: TAction;
    actTestGetApiVersion: TAction;
    actTestLogout: TAction;
    actTestLogin: TAction;
    alMain: TActionList;
    btnTestLogin: TButton;
    actFileExit: TFileExit;
    btnTestLogout: TButton;
    btnFileExit: TButton;
    btnTestGetAPIVersion: TButton;
    btnTestGetMinAPiVersion: TButton;
    btnTestGetqBitTorrentVersion: TButton;
    DividerBevel1: TDividerBevel;
    mnuTestGetqBitTorrentVersion: TMenuItem;
    mnuTestGetMinApiVersion: TMenuItem;
    mnuTestGetAPIVersion: TMenuItem;
    mnuSep1: TMenuItem;
    mnuTestLogout: TMenuItem;
    mnuTestLogin: TMenuItem;
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
    procedure actTestGetApiVersionExecute(Sender: TObject);
    procedure actTestGetMinApiVersionExecute(Sender: TObject);
    procedure actTestGetqBitTorrentVersionExecute(Sender: TObject);
    procedure actTestLoginExecute(Sender: TObject);
    procedure actTestLogoutExecute(Sender: TObject);
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

procedure TfrmMain.actTestLoginExecute(Sender: TObject);
var
  bLoginResult: Boolean;
begin
  Log('About to login.');
  //qbttMain.UserName := 'qBitTorrentWebUI';
  //qbttMain.Password := 'Password';
  try
    bLoginResult := qbttMain.Login;
    if bLoginResult then
    begin
      Log(#9'Success.');
      Info('Cookies:');
      Info(qbttMain.LoginCookie);
    end
    else
    begin
      Log('Login failed.');
    end;
  except
    on E:Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TfrmMain.actTestLogoutExecute(Sender: TObject);
var
  bLogoutResult: Boolean;
begin
  Log('About to logout.');
  try
    bLogoutResult := qbttMain.Logout;
    if bLogoutResult then
    begin
      Log(#9'Success.');
    end
    else
    begin
      Log('Logout failed.');
    end;
  except
    on E:Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TfrmMain.actTestGetApiVersionExecute(Sender: TObject);
var
  iAPIVersion: String;
begin
  Log('Getting API Version.');
  try
    iAPIVersion := qbttMain.GetApiVersion;
    Log(#9'Success.');
    Info('API Version: ' + iAPIVersion);
  except
    on E:Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TfrmMain.actTestGetMinApiVersionExecute(Sender: TObject);
var
  iMinAPIVersion: String;
begin
  Log('Getting Minimum API Version.');
  try
    iMinAPIVersion := qbttMain.GetMinApiVersion;
    Log(#9'Success.');
    Info('Min API Version: ' + iMinAPIVersion);
  except
    on E:Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TfrmMain.actTestGetqBitTorrentVersionExecute(Sender: TObject);
var
  iqBitTorrentAPIVersion: String;
begin
  Log('Getting qBitTorrent Version.');
  try
    iqBitTorrentAPIVersion := qbttMain.GetqBitTorrentVersion;
    Log(#9'Success.');
    Info('qBitTorrent Version: ' + iqBitTorrentAPIVersion);
  except
    on E:Exception do
      Log('Error: ' + E.Message);
  end;
end;

end.

