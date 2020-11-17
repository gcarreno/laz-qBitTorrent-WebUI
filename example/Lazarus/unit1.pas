unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  PairSplitter, ExtCtrls, StdCtrls, Menus, ActnList, StdActns, Spin,
  qBitTorrentWebUI, qBUtils, qBTorrents, qBTorrentsFilters;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actFileSetActive: TAction;
    actTestResumeTorrentsAll: TAction;
    actTestResumeTorrent: TAction;
    actTestPauseTorrent: TAction;
    actTestPauseTorrentsAll: TAction;
    actTestGetTorrentWebSeeds: TAction;
    actTestGetTorrentFiles: TAction;
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
    btnTestGetTorrentWebSeeds: TButton;
    btnTestGetTorrentFiles: TButton;
    btnTestPauseTorrentsAll: TButton;
    btnTestPauseTorrent: TButton;
    btnTestResumeTorrentsAll: TButton;
    btnTestResumeTorrent: TButton;
    chkFileActive: TCheckBox;
    divbTorrents: TDividerBevel;
    divbApplication: TDividerBevel;
    edtPassword: TEdit;
    edtLogin: TEdit;
    edtHost: TEdit;
    lblPassword: TLabel;
    lblLogin: TLabel;
    lblPort: TLabel;
    lblHost: TLabel;
    mnuTestGetTorrentWebSeeds: TMenuItem;
    mnuTestGetTorrentFiles: TMenuItem;
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
    edtPort: TSpinEdit;
    stLabelLog: TStaticText;
    stLabelInfo: TStaticText;
    procedure actFileSetActiveExecute(Sender: TObject);

    //Application
    procedure actTestExecShutdownExecute(Sender: TObject);

    //Torrents
    procedure actTestGetTorrentsExecute(Sender: TObject);
    procedure actTestGetTorrentFilesExecute(Sender: TObject);
    procedure actTestGetTorrentPropertiesExecute(Sender: TObject);
    procedure actTestGetTorrentTrackersExecute(Sender: TObject);
    procedure actTestGetTorrentWebSeedsExecute(Sender: TObject);

    procedure actTestPauseTorrentExecute(Sender: TObject);
    procedure actTestPauseTorrentsAllExecute(Sender: TObject);

    procedure actTestResumeTorrentsAllExecute(Sender: TObject);
    procedure actTestResumeTorrentExecute(Sender: TObject);

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetActions;
    procedure SetEndpoint;

    procedure DisableAll;
    procedure EnableAll;

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
  edtHost.Text := qbttMain.Host;
  edtPort.Value := qbttMain.Port;
  edtLogin.Text := qbttMain.UserName;
  edtPassword.Text := qbttMain.Password;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := not qbttMain.Active;
  if not CanClose then
  begin
    ShowMessage('Please De-Activate the component');
  end;
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
  lblHost.Enabled := not chkFileActive.Checked;
  edtHost.Enabled := not chkFileActive.Checked;
  lblPort.Enabled := not chkFileActive.Checked;
  edtPort.Enabled := not chkFileActive.Checked;
  lblLogin.Enabled := not chkFileActive.Checked;
  edtLogin.Enabled := not chkFileActive.Checked;
  lblPassword.Enabled := not chkFileActive.Checked;
  edtPassword.Enabled := not chkFileActive.Checked;

  //Application
  actTestExecShutdown.Enabled := chkFileActive.Checked;

  //Torrents
  actTestGetTorrents.Enabled := chkFileActive.Checked;
  actTestGetTorrentProperties.Enabled := qbttMain.Torrents.Count > 0;
  actTestGetTorrentTrackers.Enabled := qbttMain.Torrents.Count > 0;
  actTestGetTorrentWebSeeds.Enabled := qbttMain.Torrents.Count > 0;
  actTestGetTorrentFiles.Enabled := qbttMain.Torrents.Count > 0;
  actTestPauseTorrentsAll.Enabled:= chkFileActive.Checked;
  actTestPauseTorrent.Enabled:= qbttMain.Torrents.Count > 0;
  actTestResumeTorrentsAll.Enabled:= chkFileActive.Checked;
  actTestResumeTorrent.Enabled:= qbttMain.Torrents.Count > 0;
end;

procedure TfrmMain.SetEndpoint;
begin
  qbttMain.Host := edtHost.Text;
  qbttMain.Port := edtPort.Value;
  qbttMain.UserName := edtLogin.Text;
  qbttMain.Password := edtPassword.Text;
end;

procedure TfrmMain.DisableAll;
begin
  actFileSetActive.Enabled := False;

  //Application
  actTestExecShutdown.Enabled := False;

  //Torrents
  actTestGetTorrents.Enabled := False;
  actTestGetTorrentProperties.Enabled := False;
  actTestGetTorrentTrackers.Enabled := False;
  actTestGetTorrentWebSeeds.Enabled := False;
  actTestGetTorrentFiles.Enabled := False;
  actTestPauseTorrentsAll.Enabled:= False;
  actTestPauseTorrent.Enabled:= False;
  actTestResumeTorrentsAll.Enabled:= False;
  actTestResumeTorrent.Enabled:= False;

  Application.ProcessMessages;
end;

procedure TfrmMain.EnableAll;
begin
  actFileSetActive.Enabled := True;

  //Application
  actTestExecShutdown.Enabled := True;

  //Torrents
  actTestGetTorrents.Enabled := True;
  actTestGetTorrentProperties.Enabled := qbttMain.Torrents.Count > 0;
  actTestGetTorrentTrackers.Enabled := qbttMain.Torrents.Count > 0;
  actTestGetTorrentWebSeeds.Enabled := qbttMain.Torrents.Count > 0;
  actTestGetTorrentFiles.Enabled := qbttMain.Torrents.Count > 0;
  actTestPauseTorrentsAll.Enabled:= True;
  actTestPauseTorrent.Enabled:= qbttMain.Torrents.Count > 0;
  actTestResumeTorrentsAll.Enabled:= True;
  actTestResumeTorrent.Enabled:= qbttMain.Torrents.Count > 0;


  Application.ProcessMessages;
end;

procedure TfrmMain.actFileSetActiveExecute(Sender: TObject);
begin
  if chkFileActive.Checked then
    Log('Active: True')
  else
    Log('Active: False');
  SetEndpoint;
  qbttMain.Active := chkFileActive.Checked;
  SetActions;
  if chkFileActive.Checked then
  begin
    Info('API Version: ' + qbttMain.APIVersion);
    Info('qBitTorrent Version: ' + qbttMain.qBitTorrentVersion);
  end;
end;

procedure TfrmMain.actTestExecShutdownExecute(Sender: TObject);
var
  bShutdownResult: Boolean;
begin
  DisableAll;
  Log('Shutting client down.');
  try
    try
      SetEndpoint;
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
  finally
    EnableAll;
  end;
end;

procedure TfrmMain.actTestGetTorrentFilesExecute(Sender: TObject);
var
  bGetTorrentFilesResult: Boolean;
  oTorrent: TqBTorrent;
  index: Integer;
begin
  DisableAll;
  try
    if qbttMain.Torrents.Count > 0 then
    begin
      oTorrent := qbttMain.Torrents[0];
      Log('Asking torrent files for hash "' + oTorrent.Hash + '"');
      try
        SetEndpoint;
        bGetTorrentFilesResult := qbttMain.GetTorrentFiles(oTorrent.Hash);
        if bGetTorrentFilesResult then
        begin
          Log(#9'Success.');
          Info('========Files');
          Info(
            Format(
              'Torrent: %s (%s)',
              [
                oTorrent.Name,
                oTorrent.Hash
              ]
            )
          );
          for index := 0 to oTorrent.Files.Count - 1 do
          begin
            Info(
              Format(
                #9'%.2d Name: %s',
                [
                  index,
                  oTorrent.Files[index].Name
                ]
              )
            );
            Info(
              Format(
                #9'%.2d Size: %s',
                [
                  index,
                  FormatBytes(oTorrent.Files[index].Size)
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
  finally
    EnableAll;
  end;
end;

procedure TfrmMain.actTestGetTorrentPropertiesExecute(Sender: TObject);
var
  bGetTorrentPropertiesResult: Boolean;
  oTorrent: TqBTorrent;
begin
  DisableAll;
  try
    if qbttMain.Torrents.Count > 0 then
    begin
      oTorrent := qbttMain.Torrents[0];
      Log('Asking torrent properties for hash "' + oTorrent.Hash + '"');
      try
        SetEndpoint;
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
  finally
    EnableAll;
  end;
end;

procedure TfrmMain.actTestGetTorrentsExecute(Sender: TObject);
var
  bGetTorrentsResult: Boolean;
  index: Integer;
  oFilter: TqBTorrentsFilter;
begin
  DisableAll;
  Log('Asking for torrents with filters.');
  try
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
        SetEndpoint;
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
  finally
    EnableAll;
  end;
end;

procedure TfrmMain.actTestGetTorrentTrackersExecute(Sender: TObject);
var
  bGetTorrentTrackersResult: Boolean;
  oTorrent: TqBTorrent;
  index: Integer;
begin
  DisableAll;
  try
    if qbttMain.Torrents.Count > 0 then
    begin
      oTorrent := qbttMain.Torrents[0];
      Log('Asking torrent trackers for hash "' + oTorrent.Hash + '"');
      try
        SetEndpoint;
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
                #9'%.2d Url: %s',
                [
                  index,
                  oTorrent.Trackers[index].Url
                ]
              )
            );
            Info(
              Format(
                #9'%.2d Status: %s',
                [
                  index,
                  FormatTrackerStatus(oTorrent.Trackers[index].Status)
                ]
              )
            );
            Info(
              Format(
                #9'%.2d Peers: %d',
                [
                  index,
                  oTorrent.Trackers[index].NumPeers
                ]
              )
            );
            Info(
              Format(
                #9'%.2d Msg: %s',
                [
                  index,
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
  finally
    EnableAll;
  end;
end;

procedure TfrmMain.actTestGetTorrentWebSeedsExecute(Sender: TObject);
var
  bGetTorrentWebSeedsResult: Boolean;
  oTorrent: TqBTorrent;
  index: Integer;
begin
  DisableAll;
  try
    if qbttMain.Torrents.Count > 0 then
    begin
      oTorrent := qbttMain.Torrents[0];
      Log('Asking torrent web seeds for hash "' + oTorrent.Hash + '"');
      try
        SetEndpoint;
        bGetTorrentWebSeedsResult := qbttMain.GetTorrentWebSeeds(oTorrent.Hash);
        if bGetTorrentWebSeedsResult then
        begin
          Log(#9'Success.');
          Info('========Web Seeds');
          Info(
            Format(
              'Torrent: %s (%s)',
              [
                oTorrent.Name,
                oTorrent.Hash
              ]
            )
          );
          for index := 0 to oTorrent.WebSeeds.Count - 1 do
          begin
            Info(
              Format(
                #9'%.2d Url: %s',
                [
                  index,
                  oTorrent.WebSeeds[index]
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
  finally
    EnableAll;
  end;
end;

procedure TfrmMain.actTestPauseTorrentExecute(Sender: TObject);
var
  hashes: TStringList;
  oTorrent: TqBTorrent;
begin
  DisableAll;
  hashes:= TStringList.Create;
  oTorrent:= qbttMain.Torrents[0];
  Log('Pausing torrent with hash "' + oTorrent.Hash + '"');
  try
    try
      hashes.Add(oTorrent.Hash);
      qbttMain.PauseTorrents(hashes);
      Log(#9'Success');
    except
      on E:Exception do
        Log('Error: ' + E.Message);
    end;
  finally
    hashes.Free;
    EnableAll;
  end;
end;

procedure TfrmMain.actTestPauseTorrentsAllExecute(Sender: TObject);
begin
  DisableAll;
  Log('Pausing all torrents');
  try
    try
     qbttMain.PauseTorrentsAll;
     Log(#9'Success');
    except
      on E:Exception do
        Log('Error: ' + E.Message);
    end;
  finally
    EnableAll;
  end;
end;

procedure TfrmMain.actTestResumeTorrentsAllExecute(Sender: TObject);
begin
  DisableAll;
  Log('Resuming all torrents');
  try
    try
     qbttMain.ResumeTorrentsAll;
     Log(#9'Success');
    except
      on E:Exception do
        Log('Error: ' + E.Message);
    end;
  finally
    EnableAll;
  end;
end;

procedure TfrmMain.actTestResumeTorrentExecute(Sender: TObject);
var
  hashes: TStringList;
  oTorrent: TqBTorrent;
begin
  DisableAll;
  hashes:= TStringList.Create;
  oTorrent:= qbttMain.Torrents[0];
  Log('Resuming torrent with hash "' + oTorrent.Hash + '"');
  try
    try
      hashes.Add(oTorrent.Hash);
      qbttMain.ResumeTorrents(hashes);
      Log(#9'Success');
    except
      on E:Exception do
        Log('Error: ' + E.Message);
    end;
  finally
    hashes.Free;
    EnableAll;
  end;
end;

end.

