unit TestTqBTorrents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry, fpjson,
  qBCommon, qBTorrents;

type

  { TTestTqBTorrents }

  TTestTqBTorrents = class(TTestCase)
  private
    FqBTorrents: TqBTorrents;

    FTorrentsText: TStringList;
    FTorrentsStream: TFileStream;
    FjData: TJSONData;

    FDataPath: String;

    procedure LoadJSON(const AFile: String);
    procedure LoadJSONData(const AFile: String);
    procedure LoadStream(const AFile: String);

    procedure TestTorrentsLoad;
  protected
    //procedure SetUp; override;
    //procedure TearDown; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    // Create
    procedure TestTorrentsCreate;

    // Load Torrents
    procedure TestTorrentsLoadFromJSON;
    procedure TestTorrentsLoadFromJSONData;
    procedure TestTorrentsLoadFromJSONArray;
    procedure TestTorrentsLoadFromStream;

    // Update Torrents
    { DONE 1 -ogcarreno -cTqBTorrents -p1 : Complete Update Torrents }
    procedure TestTorrentsUpdateFromJSON;
    procedure TestTorrentsUpdateFromJSONData;
    procedure TestTorrentsUpdateFromJSONArray;
    procedure TestTorrentsUpdateFromStream;

    // Update Torrent by Hash
    { TODO 2 -ogcarreno -cTqBTorrents : Complete Update Torrent By Hash }
    procedure TestTorrentUpdateFromJSON;
    procedure TestTorrentUpdateFromJSONData;
    procedure TestTorrentUpdateFromJSONObject;
    procedure TestTorrentUpdateFromStream;

    // Delete Torrent by Hash
    { TODO 3 -ogcarreno -cTqBTorrents : Complete Delete Torrent By Hash }

  end;

implementation

procedure TTestTqBTorrents.LoadJSON(const AFile: String);
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

procedure TTestTqBTorrents.LoadJSONData(const AFile: String);
begin
  LoadJSON(AFile);
  FjData := GetJSONData(FTorrentsText.Text);
end;

procedure TTestTqBTorrents.LoadStream(const AFile: String);
begin
  if FileExists(FDataPath + AFile) then
  begin
    FTorrentsStream := TFileStream.Create(FDataPath + AFile, fmOpenRead);
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoad;
begin
  AssertEquals('Loaded torrents 3', 3, FqBTorrents.Count);
  AssertEquals('Loaded #1 Hash',
    '0403fb4728bd788fbcb67e87d6feb241ef38c75a',
    FqBTorrents[0].Hash);
  AssertEquals('Loaded #2 Hash',
    '34930674ef3bb9317fb5f263cca830f52685235b',
    FqBTorrents[1].Hash);
  AssertEquals('Loaded #3 Hash',
    'da775e4aaf5635ef72583a391977e5ed6f14617e',
    FqBTorrents[2].Hash);
end;

//procedure TTestTqBTorrents.SetUp;
//begin
//end;
//
//procedure TTestTqBTorrents.TearDown;
//begin
//end;

constructor TTestTqBTorrents.Create;
begin
  inherited Create;
  FDataPath := ExtractFileDir(ParamStr(0)) + '/../tests/data/';
  FTorrentsText := TStringList.Create;
end;

destructor TTestTqBTorrents.Destroy;
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

procedure TTestTqBTorrents.TestTorrentsCreate;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    AssertEquals('Torrents Count 0', 0, FqBTorrents.Count);
  finally
    FreeAndNil(FqBTorrents);
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoadFromJSON;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadJSON('torrents.json');
    FqBTorrents.LoadTorrents(FTorrentsText.Text);
    TestTorrentsLoad;
  finally
    FreeAndNil(FqBTorrents);
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoadFromJSONData;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadJSONData('torrents.json');
    try
      FqBTorrents.LoadTorrents(FjData);
      TestTorrentsLoad;
    finally
      if Assigned(FjData) then
        FjData.Free;
    end;
  finally
    FreeAndNil(FqBTorrents);
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoadFromJSONArray;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadJSONData('torrents.json');
    FqBTorrents.LoadTorrents(FjData as TJSONArray);
    TestTorrentsLoad;
  finally
    FreeAndNil(FqBTorrents);
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoadFromStream;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadStream('torrents.json');
    FqBTorrents.LoadTorrents(FTorrentsStream);
    TestTorrentsLoad;
  finally
    FreeAndNil(FTorrentsStream);
    FreeAndNil(FqBTorrents);
  end;
end;

procedure TTestTqBTorrents.TestTorrentsUpdateFromJSON;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadJSON('torrents.json');
    FqBTorrents.LoadTorrents(FTorrentsText.Text);
    AssertEquals('Loaded torrents 3', 3, FqBTorrents.Count);

    LoadJSON('torrents-update-1.json');
    FqBTorrents.UpdateTorrents(FTorrentsText.Text);
    AssertEquals('Updated torrents 4', 4, FqBTorrents.Count);
    AssertEquals('Updated torrent 4 Name', 'ubuntu-17.04-desktop-amd64.iso', FqBTorrents[3].Name);

    LoadJSON('torrents-update-2.json');
    FqBTorrents.UpdateTorrents(FTorrentsText.Text);
    AssertEquals('Updated torrent 1 NumComplete', 1337, FqBTorrents[0].NumComplete);
  finally
    FreeAndNil(FqBTorrents);
  end;
end;

procedure TTestTqBTorrents.TestTorrentsUpdateFromJSONData;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadJSONData('torrents.json');
    FqBTorrents.LoadTorrents(FjData);
    AssertEquals('Loaded torrents 3', 3, FqBTorrents.Count);

    LoadJSONData('torrents-update-1.json');
    FqBTorrents.UpdateTorrents(FjData);
    AssertEquals('Updated torrents 4', 4, FqBTorrents.Count);
    AssertEquals('Updated torrent 4 Name', 'ubuntu-17.04-desktop-amd64.iso', FqBTorrents[3].Name);

    LoadJSONData('torrents-update-2.json');
    FqBTorrents.UpdateTorrents(FjData);
    AssertEquals('Updated torrent 1 NumComplete', 1337, FqBTorrents[0].NumComplete);
  finally
    FreeAndNil(FqBTorrents);
  end;
end;

procedure TTestTqBTorrents.TestTorrentsUpdateFromJSONArray;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadJSONData('torrents.json');
    FqBTorrents.LoadTorrents(FjData as TJSONArray);
    AssertEquals('Loaded torrents 3', 3, FqBTorrents.Count);

    LoadJSONData('torrents-update-1.json');
    FqBTorrents.UpdateTorrents(FjData as TJSONArray);
    AssertEquals('Updated torrents 4', 4, FqBTorrents.Count);
    AssertEquals('Updated torrent 4 Name', 'ubuntu-17.04-desktop-amd64.iso', FqBTorrents[3].Name);

    LoadJSONData('torrents-update-2.json');
    FqBTorrents.UpdateTorrents(FjData as TJSONArray);
    AssertEquals('Updated torrent 1 NumComplete', 1337, FqBTorrents[0].NumComplete);
  finally
    FreeAndNil(FqBTorrents);
  end;
end;

procedure TTestTqBTorrents.TestTorrentsUpdateFromStream;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadStream('torrents.json');
    FqBTorrents.LoadTorrents(FTorrentsStream);
    FreeAndNil(FTorrentsStream);
    AssertEquals('Loaded torrents 3', 3, FqBTorrents.Count);

    LoadStream('torrents-update-1.json');
    FqBTorrents.UpdateTorrents(FTorrentsStream);
    FreeAndNil(FTorrentsStream);
    AssertEquals('Updated torrents 4', 4, FqBTorrents.Count);
    AssertEquals('Updated torrent 4 Name', 'ubuntu-17.04-desktop-amd64.iso', FqBTorrents[3].Name);

    LoadStream('torrents-update-2.json');
    FqBTorrents.UpdateTorrents(FTorrentsStream);
    FreeAndNil(FTorrentsStream);
    AssertEquals('Updated torrent 1 NumComplete', 1337, FqBTorrents[0].NumComplete);
  finally
    FreeAndNil(FqBTorrents);
  end;
end;

procedure TTestTqBTorrents.TestTorrentUpdateFromJSON;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadJSON('torrents.json');
    FqBTorrents.LoadTorrents(FTorrentsText.Text);
    LoadJSON('torrents-update-3.json');
    FqBTorrents.UpdateTorrent('0403fb4728bd788fbcb67e87d6feb241ef38c75a', FTorrentsText.Text);
    AssertEquals('Updated torrent 1 NumComplete', 1337, FqBTorrents[0].NumComplete);
  finally
    FreeAndNil(FqBTorrents);
  end;
end;

procedure TTestTqBTorrents.TestTorrentUpdateFromJSONData;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadJSONData('torrents.json');
    FqBTorrents.LoadTorrents(FjData);
    LoadJSONData('torrents-update-3.json');
    FqBTorrents.UpdateTorrent('0403fb4728bd788fbcb67e87d6feb241ef38c75a', FjData);
    AssertEquals('Updated torrent 1 NumComplete', 1337, FqBTorrents[0].NumComplete);
  finally
    FreeAndNil(FqBTorrents);
  end;
end;

procedure TTestTqBTorrents.TestTorrentUpdateFromJSONObject;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadJSONData('torrents.json');
    FqBTorrents.LoadTorrents(FjData);
    LoadJSONData('torrents-update-3.json');
    FqBTorrents.UpdateTorrent('0403fb4728bd788fbcb67e87d6feb241ef38c75a', FjData as TJSONObject);
    AssertEquals('Updated torrent 1 NumComplete', 1337, FqBTorrents[0].NumComplete);
  finally
    FreeAndNil(FqBTorrents);
  end;
end;

procedure TTestTqBTorrents.TestTorrentUpdateFromStream;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadStream('torrents.json');
    FqBTorrents.LoadTorrents(FTorrentsStream);
    FreeAndNil(FTorrentsStream);

    LoadStream('torrents-update-3.json');
    FqBTorrents.UpdateTorrent('0403fb4728bd788fbcb67e87d6feb241ef38c75a', FTorrentsStream);
    FreeAndNil(FTorrentsStream);
    AssertEquals('Updated torrent 1 NumComplete', 1337, FqBTorrents[0].NumComplete);
  finally
    FreeAndNil(FqBTorrents);
  end;
end;

initialization
  RegisterTest(TTestTqBTorrents);
end.

