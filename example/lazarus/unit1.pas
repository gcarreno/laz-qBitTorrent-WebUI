unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  PairSplitter, ExtCtrls, StdCtrls, Menus, ActnList, StdActns,
  qBitTorrentWebUI, qBTorrents;

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
    function FormatBytes(aSize: Int64): String;
    function FormatState(aState: TqBTorrentState): String;
    procedure SetActions;

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

procedure TfrmMain.SetActions;
begin
  actTestExecShutdown.Enabled := chkFileActive.Checked;
  actTestGetTorrents.Enabled := chkFileActive.Checked;
end;

function TfrmMain.FormatBytes(aSize: Int64): String;
var
  dSize: Double;
begin
  Result := '';
  dSize := 0.0;
  if aSize < 1024 then
  begin
    Result := IntToStr(aSize) + 'B';
    exit;
  end;
  if aSize < (1024*1024) then
  begin
    dSize := aSize / 1024;
    Result := FormatFloat('0.##', dSize)+'KB';
    exit;
  end;
  if aSize < (1024*1024*1024) then
  begin
    dSize := aSize / 1024 / 1024;
    Result := FormatFloat('0.##', dSize)+'MB';
    exit;
  end;
  if aSize < (1024*1024*1024*1024) then
  begin
    dSize := aSize / 1024 / 1024 / 1024;
    Result := FormatFloat('0.##', dSize)+'GB';
    exit;
  end;
  if aSize < (1024*1024*1024*1024*1024) then
  begin
    dSize := aSize / 1024 / 1024 / 1024 / 1024;
    Result := FormatFloat('0.##', dSize)+'TB';
  end;
end;

function TfrmMain.FormatState(aState: TqBTorrentState): String;
begin
  Result := 'Unknown';
  case aState of
    tsError:       Result := 'Error';
    tsPausedUp:    Result := 'Paused Upload';
    tsPausedDl:    Result := 'Paused Download';
    tsQueuedUp:    Result := 'Queued Upload';
    tsQueuedDl:    Result := 'Queued Download';
    tsUploading:   Result := 'Uploading';
    tsStalledUp:   Result := 'Stalled Upload';
    tsStalledDl:   Result := 'Stalled Download';
    tsCheckingUp:  Result := 'Checking Upload';
    tsCheckingDl:  Result := 'Checking Download';
    tsDownloading: Result := 'Downloading';
    tsMetaDl:      Result := 'Downloading Metadata';
  end;
end;

procedure TfrmMain.actFileSetActiveExecute(Sender: TObject);
begin
  if chkFileActive.Checked then
    Log('Active: True')
  else
    Log('Active: False');
  qbttMain.Active := chkFileActive.Checked;
  SetActions;
  if chkFileActive.Checked then
  begin
    Info('API Version: ' + IntToStr(qbttMain.APIVersion));
    Info('Min API Version: ' + IntToStr(qbttMain.MinAPIVersion));
    Info('qBitTorrent Version: ' + qbttMain.qBitTorrentVersion);
  end;
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
    SetActions;
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
  index: Integer;
  oFilter: TqBTorrentsFilter;
begin
  Log('Asking for torrents with filters.');
  try
    oFilter := TqBTorrentsFilter.Create;
    oFilter
      .withFilter('all')
      .withSort('priority');
    Log('Filter: ' + oFilter.Filters);
    try
      bGetTorrentsResult := qbttMain.GetTorrentsFiltered(oFilter);
    finally
      oFilter.Free;
    end;
    if bGetTorrentsResult then
    begin
      Log(#9'Success.');
      Info('Got: ' + IntToStr(qbttMain.Torrents.Count) +' torrents');
      for index := 0 to qbttMain.Torrents.Count - 1 do
      begin
        Info(
          Format(
            'Torrent: %s (%s)',
            [
              qbttMain.Torrents[index].Name,
              qbttMain.Torrents[index].Hash
            ]
          )
        );
        Info(Format(#9'#: %d', [qbttMain.Torrents[index].Priority]));
        Info(#9'Size: '+FormatBytes(qbttMain.Torrents[index].Size));
        Info(#9'Progress: '+FormatFloat('##0.00%', qbttMain.Torrents[index].Progress));
        Info(#9'DL Speed: '+FormatBytes(qbttMain.Torrents[index].DlSpeed)+'/s');
        Info(#9'UP Speed: '+FormatBytes(qbttMain.Torrents[index].UpSpeed)+'/s');
        Info(#9'State: '+FormatState(qbttMain.Torrents[index].State));
        Info(Format(#9'Seeds: %d', [qbttMain.Torrents[index].NumSeeds]));
        Info(Format(#9'Leech: %d', [qbttMain.Torrents[index].NumLeechs]));
        //Info(#9': '+qbttMain.Torrents[index].);
        //Info(#9': '+qbttMain.Torrents[index].);
        //Info(#9': '+qbttMain.Torrents[index].);
        //Info(#9': '+qbttMain.Torrents[index].);
        //Info(#9': '+qbttMain.Torrents[index].);
        //Info(#9': '+qbttMain.Torrents[index].);
        //Info(#9': '+qbttMain.Torrents[index].);
        //Info(#9': '+qbttMain.Torrents[index].);
        //Info(#9': '+qbttMain.Torrents[index].);
      end;
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

