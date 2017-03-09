unit TestTqBTorrents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry, fpjson,
  jsonparser, jsonscanner,
  qBTorrents;

type

  { TTestTqBTorrents }

  TTestTqBTorrents = class(TTestCase)
  private
    FqBTorrents: TqBTorrents;

    FTorrentsText: TStringList;
    FTorrentsStream: TFileStream;
    FjParser: TJSONParser;
    FjData: TJSONData;

    FDataPath: String;

    procedure LoadData(const AFile: String);
    procedure LoadDataStream(const AFile: String);
    procedure LoadJSONData(const AFile: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Create
    procedure TestTorrentsCreate;

    // Load Torrents
    procedure TestTorrentsLoadFromJSON;
    procedure TestTorrentsLoadFromJSONData;
    procedure TestTorrentsLoadFromJSONArray;
    procedure TestTorrentsLoadFromStream;

    // Update Torrents
    { TODO 1 -ogcarreno -cTqBTorrents -p1 : Complete Update Torrents }
    procedure TestTorrentsUpdateFromJSON;
    procedure TestTorrentsUpdateFromJSONData;
    procedure TestTorrentsUpdateFromJSONArray;
    procedure TestTorrentsUpdateFromSteam;

    // Update Torrent by Hash
    { TODO 2 -ogcarreno -cTqBTorrents : Complete Update Torrent By Hash }

    // Delete Torrent by Hash
    { TODO 3 -ogcarreno -cTqBTorrents : Complete Delete Torrent By Hash }

  end;

implementation

procedure TTestTqBTorrents.LoadData(const AFile: String);
begin
  if not Assigned(FTorrentsText) then
  begin
    FTorrentsText := TStringList.Create;
  end;
  if FileExists(FDataPath + AFile) then
  begin
    FTorrentsText.LoadFromFile(FDataPath + AFile);
  end
  else
  begin
    FTorrentsText.Clear;
  end;
end;

procedure TTestTqBTorrents.LoadDataStream(const AFile: String);
begin
  if FileExists(FDataPath + AFile) then
  begin
    FTorrentsStream := TFileStream.Create(FDataPath + AFile, fmOpenRead);
  end;
end;

procedure TTestTqBTorrents.LoadJSONData(const AFile: String);
begin
  LoadData(AFile);
{$IF FPC_FULLVERSION >= 30002}
  FjParser := TJSONParser.Create(FTorrentsText.Text, [joUTF8, joIgnoreTrailingComma]);
{$ELSE}
  FjParser := TJSONParser.Create(FTorrentText.Text, True);
{$ENDIF}
  try
    FjData := FjParser.Parse;
  finally
    FjParser.Free;
  end;
end;

procedure TTestTqBTorrents.SetUp;
begin
  FDataPath := ExtractFileDir(ParamStr(0)) + '/../tests/data/';
  FTorrentsText := TStringList.Create;
end;

procedure TTestTqBTorrents.TearDown;
begin
  if Assigned(FTorrentsText) then
  begin
    FTorrentsText.Free;
  end;
  if Assigned(FTorrentsStream) then
  begin
    FTorrentsStream.Free;
  end;
end;

procedure TTestTqBTorrents.TestTorrentsCreate;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    AssertEquals('Torrents Count 0', 0, FqBTorrents.Count);
  finally
    FqBTorrents.Free;
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoadFromJSON;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadData('torrents.json');
    FqBTorrents.LoadTorrents(FTorrentsText.Text);
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
  finally
    FqBTorrents.Free;
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoadFromJSONData;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadJSONData('torrents.json');
    try
      FqBTorrents.LoadTorrents(FjData);
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
    finally
      if Assigned(FjData) then
        FjData.Free;
    end;
  finally
    FqBTorrents.Free;
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoadFromJSONArray;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadJSONData('torrents.json');
    try
      FqBTorrents.LoadTorrents(FjData as TJSONArray);
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
    finally
      if Assigned(FjData) then
        FjData.Free;
    end;
  finally
    FqBTorrents.Free;
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoadFromStream;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadDataStream('torrents.json');
    FqBTorrents.LoadTorrents(FTorrentsStream);
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
  finally
    FqBTorrents.Free;
  end;
end;

procedure TTestTqBTorrents.TestTorrentsUpdateFromJSON;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadData('torrents.json');
    FqBTorrents.LoadTorrents(FTorrentsText.Text);
    AssertEquals('Loaded torrents 3', 3, FqBTorrents.Count);

    LoadData('torrents-update-1.json');
    FqBTorrents.UpdateTorrents(FTorrentsText.Text);
    AssertEquals('Updated torrents 4', 4, FqBTorrents.Count);
    AssertEquals('Updated torrent 4 Name', 'ubuntu-17.04-desktop-amd64.iso', FqBTorrents[3].Name);

    LoadData('torrents-update-2.json');
    FqBTorrents.UpdateTorrents(FTorrentsText.Text);
    AssertEquals('Updated torrent 1 NumComplete', 1337, FqBTorrents[0].NumComplete);
  finally
    FqBTorrents.Free;
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
    FqBTorrents.Free;
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
    FqBTorrents.Free;
  end;
end;

procedure TTestTqBTorrents.TestTorrentsUpdateFromSteam;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    LoadDataStream('torrents.json');
    FqBTorrents.LoadTorrents(FTorrentsStream);
    AssertEquals('Loaded torrents 3', 3, FqBTorrents.Count);

    LoadDataStream('torrents-update-1.json');
    FqBTorrents.UpdateTorrents(FTorrentsStream);
    AssertEquals('Updated torrents 4', 4, FqBTorrents.Count);
    AssertEquals('Updated torrent 4 Name', 'ubuntu-17.04-desktop-amd64.iso', FqBTorrents[3].Name);

    LoadDataStream('torrents-update-2.json');
    FqBTorrents.UpdateTorrents(FTorrentsStream);
    AssertEquals('Updated torrent 1 NumComplete', 1337, FqBTorrents[0].NumComplete);
  finally
    FqBTorrents.Free;
  end;
end;

initialization
  RegisterTest(TTestTqBTorrents);
end.

