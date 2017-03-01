unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  PairSplitter, ExtCtrls, StdCtrls, Menus, ActnList, StdActns,
  qBitTorrentWebUI, qBUtils, qBTorrents, qBTorrentsFilters;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actFileSetActive: TAction;
    actTestGetTorrentTrackers: TAction;
    actTestGetTorrentProperties: TAction;
    actTestGetTorrents: TAction;
    actTestExecShutdown: TAction;
    alMain: TActionList;
    actFileExit: TFileExit;
    btnFileExit: TButton;
    btnTestExecShutdown: TButton;
    btnTestGetTorrents: TButton;
    btnTestGetTorrentProperties: TButton;
    btnTestGetTorrentTrackers: TButton;
    chkFileActive: TCheckBox;
    divbGetMethhods: TDividerBevel;
    divbCommands: TDividerBevel;
    mnuTestGetTorrentTrackers: TMenuItem;
    mnuTestGetTorrentProperties: TMenuItem;
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
    procedure actTestGetTorrentPropertiesExecute(Sender: TObject);
    procedure actTestGetTorrentsExecute(Sender: TObject);
    procedure actTestGetTorrentTrackersExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
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
  actTestGetTorrentProperties.Enabled := chkFileActive.Checked;
  actTestGetTorrentTrackers.Enabled := chkFileActive.Checked;
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
    // If we don't do this it will trigger actFileSetActive ...
    chkFileActive.Action := nil;
    chkFileActive.Checked := False;
    SetActions;
    chkFileActive.Action := actFileSetActive;
    // ... on a frickin loop
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

procedure TfrmMain.actTestGetTorrentPropertiesExecute(Sender: TObject);
var
  bGetTorrentPropertiesResult: Boolean;
  oTorrent: TqBTorrent;
begin
  if qbttMain.Torrents.Count > 0 then
  begin
    oTorrent := qbttMain.Torrents[0];
    Log('Asking torrent properties for hash "' + oTorrent.Hash + '"');
    try
      bGetTorrentPropertiesResult := qbttMain.GetTorrentProperties(oTorrent.Hash);
      if bGetTorrentPropertiesResult then
      begin
        Log(#9'Success.');
        Info('========Properties');
        Info(
          Format(
            'Torrent: %s (%s)',
            [
              oTorrent.Name,
              oTorrent.Hash
            ]
          )
        );
        Info(
          Format(
            #9'Save Path: %s',
            [
              oTorrent.Properties.SavePath
            ]
          )
        );
        Info(
          Format(
            #9'Added: %s',
            [
              FormatDateTime(
                'YYYY/MM/DD HH:MM:SS',
                oTorrent.Properties.AdditionDate
              )
            ]
          )
        );
        Info(
          Format(
            #9'Comment: "%s"',
            [
              oTorrent.Properties.Comment
            ]
          )
        );
      end
      else
      begin
        Log(#9'Failed.');
      end;
    except
      on E:Exception do
        Log('Error: ' + E.Message);
    end;
  end
  else
  begin
    Log('Torrent list is empty');
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
      .withSort('priority')
      //.withLimit(10)
      //.withReverse(True)
      ;
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
        Info('========Torrents');
        Info(
          Format(
            'Torrent: %s (%s)',
            [
              qbttMain.Torrents[index].Name,
              qbttMain.Torrents[index].Hash
            ]
          )
        );
        Info(
          Format(
            #9'#: %d',
            [
              qbttMain.Torrents[index].Priority
            ]
          )
        );
        Info(
          Format(
            #9'Size: %s',
            [
              FormatBytes(qbttMain.Torrents[index].Size)
            ]
          )
        );
        Info(
          Format(
            #9'Progress: %s',
            [
              FormatFloat('##0.00%', qbttMain.Torrents[index].Progress)
            ]
          )
        );
        Info(
          Format(
            #9'DL Speed: %s',
            [
              FormatBytesPerSecond(qbttMain.Torrents[index].DlSpeed)
            ]
          )
        );
        Info(
          Format(
            #9'UP Speed: %s',
            [
              FormatBytesPerSecond(qbttMain.Torrents[index].UpSpeed)
            ]
          )
        );
        Info(
          Format(
            #9'State: %s',
            [
              FormatTorrentState(qbttMain.Torrents[index].State)
            ]
          )
        );
        Info(
          Format(
            #9'Seeds: %d',
            [
              qbttMain.Torrents[index].NumSeeds
            ]
          )
        );
        Info(
          Format(
            #9'Leech: %d',
            [
              qbttMain.Torrents[index].NumLeechs
            ]
          )
        );
        Info(
          Format(
            #9'Added: %s',
            [
              FormatDateTime(
                'YYYY/MM/DD HH:MM:SS',
                qbttMain.Torrents[index].AddedOn
              )
            ]
          )
        );
        Info(
          Format(
            #9'Completion: %s',
            [
              FormatDateTime(
                'YYYY/MM/DD HH:MM:SS',
                qbttMain.Torrents[index].CompletionOn
              )
            ]
          )
        );
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

procedure TfrmMain.actTestGetTorrentTrackersExecute(Sender: TObject);
var
  bGetTorrentTrackersResult: Boolean;
  oTorrent: TqBTorrent;
  index: Integer;
begin
  if qbttMain.Torrents.Count > 0 then
  begin
    oTorrent := qbttMain.Torrents[0];
    Log('Asking torrent trackers for hash "' + oTorrent.Hash + '"');
    try
      bGetTorrentTrackersResult := qbttMain.GetTorrentTrackers(oTorrent.Hash);
      if bGetTorrentTrackersResult then
      begin
        Log(#9'Success.');
        Info('========Trackers');
        Info(
          Format(
            'Torrent: %s (%s)',
            [
              oTorrent.Name,
              oTorrent.Hash
            ]
          )
        );
        for index := 0 to oTorrent.Trackers.Count - 1 do
        begin
          Info(
            Format(
              #9'Url: %s',
              [
                oTorrent.Trackers[index].Url
              ]
            )
          );
          Info(
            Format(
              #9'Status: %s',
              [
                FormatTrackerStatus(oTorrent.Trackers[index].Status)
              ]
            )
          );
          Info(
            Format(
              #9'Peers: %d',
              [
                oTorrent.Trackers[index].NumPeers
              ]
            )
          );
          Info(
            Format(
              #9'Msg: %s',
              [
                oTorrent.Trackers[index].Msg
              ]
            )
          );
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
  end
  else
  begin
    Log('Torrent list is empty');
  end;
end;

end.

