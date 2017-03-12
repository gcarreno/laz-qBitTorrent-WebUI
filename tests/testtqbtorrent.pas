unit TestTqBTorrent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson,
  qBCommon, qBTorrents;

type

  { TTestTqBTorrent }

  TTestTqBTorrent = class(TTestCase)
  private
    FqBTorrent: TqBTorrent;

    FTorrentsText: TStringList;
    FTorrentsStream: TFileStream;
    FjData: TJSONData;

    FDataPath: String;

    procedure LoadJSON(const AFile: String);
    procedure LoadJSONData(const AFile: String);
    procedure LoadStream(const AFile: String);

    procedure TestTorrent1EmptyFields;
    procedure TestTorrent1Fields;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    // Create Torrent
    procedure TestTorrentCreate;
    procedure TestTorrentCreateFromJSON;
    procedure TestTorrentCreateFromJSONData;
    procedure TestTorrentCreateFromJSONObject;
    procedure TestTorrentCreateFromStream;
  end;

implementation

procedure TTestTqBTorrent.LoadJSON(const AFile: String);
begin
  if FileExists(FDataPath + AFile) then
  begin
    FTorrentsText.Clear;
    FTorrentsText.LoadFromFile(FDataPath + AFile);
  end
  else
  begin
    FTorrentsText.Clear;
  end;
end;

procedure TTestTqBTorrent.LoadJSONData(const AFile: String);
begin
  LoadJSON(AFile);
  FjData := GetJSONData(FTorrentsText.Text);
end;

procedure TTestTqBTorrent.LoadStream(const AFile: String);
begin
  if FileExists(FDataPath + AFile) then
  begin
    FTorrentsStream := TFileStream.Create(FDataPath + AFile, fmOpenRead);
  end;
end;

procedure TTestTqBTorrent.TestTorrent1EmptyFields;
begin
  { TODO 1 -ogcarreno -cTqBTorrent : Finish the filed list }
  AssertEquals('Torrent Hash', '', FqBTorrent.Hash);
  AssertEquals('Torrent Name', '', FqBTorrent.Name);
  AssertEquals('Torrent Size', 0, FqBTorrent.Size);
  AssertEquals('Torrent Progress', 0.0, FqBTorrent.Progress);
  AssertEquals('Torrent Dl Speed', 0, FqBTorrent.DlSpeed);
  AssertEquals('Torrent Up Speed', 0, FqBTorrent.UpSpeed);
  AssertEquals('Torrent Priority', -1, FqBTorrent.Priority);
  AssertEquals('Torrent Num Seeds', 0, FqBTorrent.NumSeeds);
  AssertEquals('Torrent Num Complete', 0, FqBTorrent.NumComplete);
  AssertEquals('Torrent Num Leechs', 0, FqBTorrent.NumLeechs);
  AssertEquals('Torrent Num Incomplete', 0, FqBTorrent.NumIncomplete);
  AssertEquals('Torrent Ratio', 0.0, FqBTorrent.Ratio);
  AssertEquals('Torrent ETA', 0, FqBTorrent.Eta);
  AssertEquals('Torrent State', qBTorrentStateToStr(qtsUnknown), qBTorrentStateToStr(FqBTorrent.State));
  AssertEquals('Torrent Sequential Dl', False, FqBTorrent.SeqDl);
  AssertEquals('Torrent FirstLast', False, FqBTorrent.FirstLastPiecePrioritized);
  AssertEquals('Torrent Category', '', FqBTorrent.Category);
  AssertEquals('Torrent Super Seeding', False, FqBTorrent.SuperSeeding);
  AssertEquals('Torrent Force Start', False, FqBTorrent.ForceStart);
  AssertEquals('Torrent Save Path', '', FqBTorrent.SavePath);
  AssertEquals('Torrent Added On', 0.0, FqBTorrent.AddedOn);
  AssertEquals('Torrent Completion On', 0.0, FqBTorrent.CompletionOn);
end;

procedure TTestTqBTorrent.TestTorrent1Fields;
begin
  { TODO 2 -ogcarreno -cTqBTorrent : Finish the filed list }
  AssertEquals('Torrent Hash', '0403fb4728bd788fbcb67e87d6feb241ef38c75a', FqBTorrent.Hash);
  AssertEquals('Torrent Name', 'ubuntu-16.10-desktop-amd64.iso', FqBTorrent.Name);
  AssertEquals('Torrent Size', 1593835520, FqBTorrent.Size);
  AssertEquals('Torrent Progress', 0.0, FqBTorrent.Progress);
  AssertEquals('Torrent Dl Speed', 0, FqBTorrent.DlSpeed);
  AssertEquals('Torrent Up Speed', 0, FqBTorrent.UpSpeed);
  AssertEquals('Torrent Priority', 3, FqBTorrent.Priority);
  AssertEquals('Torrent Num Seeds', 0, FqBTorrent.NumSeeds);
  AssertEquals('Torrent Num Complete', 0, FqBTorrent.NumComplete);
  AssertEquals('Torrent Num Leechs', 0, FqBTorrent.NumLeechs);
  AssertEquals('Torrent Num Incomplete', 0, FqBTorrent.NumIncomplete);
  AssertEquals('Torrent Ratio', 0.0, FqBTorrent.Ratio);
  AssertEquals('Torrent ETA', 8640000, FqBTorrent.Eta);
  AssertEquals('Torrent State', qBTorrentStateToStr(qtsPausedDl), qBTorrentStateToStr(FqBTorrent.State));
  AssertEquals('Torrent Sequential Dl', False, FqBTorrent.SeqDl);
  AssertEquals('Torrent FirstLast', False, FqBTorrent.FirstLastPiecePrioritized);
  AssertEquals('Torrent Category', 'ISO', FqBTorrent.Category);
  AssertEquals('Torrent Super Seeding', False, FqBTorrent.SuperSeeding);
  AssertEquals('Torrent Force Start', False, FqBTorrent.ForceStart);
  AssertEquals('Torrent Save Path', '/home/user/Downloads/Ubuntu 16.10/', FqBTorrent.SavePath);
  AssertEquals('Torrent Added On', StrToDateTime('5-3-17 22:56:05'), FqBTorrent.AddedOn);
  AssertEquals('Torrent Completion On', 0.0, FqBTorrent.CompletionOn);
end;

constructor TTestTqBTorrent.Create;
begin
  inherited Create;
  FDataPath := ExtractFileDir(ParamStr(0)) + '/../tests/data/';
  FTorrentsText := TStringList.Create;
end;

destructor TTestTqBTorrent.Destroy;
begin
  if Assigned(FTorrentsText) then
  begin
    FreeAndNil(FTorrentsText);
  end;
  if Assigned(FTorrentsStream) then
  begin
    FreeAndNil(FTorrentsStream);
  end;
  inherited Destroy;
end;

procedure TTestTqBTorrent.TestTorrentCreate;
begin
  FqBTorrent:= TqBTorrent.Create;
  TestTorrent1EmptyFields;
end;

procedure TTestTqBTorrent.TestTorrentCreateFromJSON;
begin
  try
    LoadJSON('torrent-1.json');
    FqBTorrent := TqBTorrent.Create(FTorrentsText.Text);
    TestTorrent1Fields;
  finally
    FqBTorrent.Free;
  end;
end;

procedure TTestTqBTorrent.TestTorrentCreateFromJSONData;
begin
  try
    LoadJSONData('torrent-1.json');
    FqBTorrent := TqBTorrent.Create(FjData as TJSONObject);
    TestTorrent1Fields;
  finally
    FqBTorrent.Free;
  end;
end;

procedure TTestTqBTorrent.TestTorrentCreateFromJSONObject;
begin
  try
    LoadJSONData('torrent-1.json');
    FqBTorrent := TqBTorrent.Create(FjData as TJSONObject);
    TestTorrent1Fields;
  finally
    FqBTorrent.Free;
  end;
end;

procedure TTestTqBTorrent.TestTorrentCreateFromStream;
begin
  try
    LoadStream('torrent-1.json');
    FqBTorrent := TqBTorrent.Create(FTorrentsStream);
    FreeAndNil(FTorrentsStream);
    TestTorrent1Fields;
  finally
    FqBTorrent.Free;
  end;
end;

initialization
  RegisterTest(TTestTqBTorrent);
end.

