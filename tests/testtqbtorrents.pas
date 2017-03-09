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

    //Update Torrents

    //Update Torrent by Hash

  end;

implementation

procedure TTestTqBTorrents.SetUp;
var
  sFileName: String;
begin
  sFileName := ExtractFileDir(ParamStr(0));
  sFileName := sFileName + '/../tests/data/torrents.json';
  FTorrentsText := TStringList.Create;
  FTorrentsText.LoadFromFile(sFileName);
  FTorrentsStream := TFileStream.Create(sFileName, fmOpenRead);
  //inherited SetUp;
end;

procedure TTestTqBTorrents.TearDown;
begin
  FTorrentsText.Free;
  FTorrentsStream.Free;
  //inherited TearDown;
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
  {$IF FPC_FULLVERSION >= 30002}
    FjParser := TJSONParser.Create(FTorrentsText.Text, [joUTF8, joIgnoreTrailingComma]);
  {$ELSE}
    FjParser := TJSONParser.Create(FTorrentText.Text, True);
  {$ENDIF}
    try
      FjData := FjParser.Parse;
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
        FjData.Free;
      end;
    finally
      FjParser.Free;
    end;
  finally
    FqBTorrents.Free;
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoadFromJSONArray;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
  {$IF FPC_FULLVERSION >= 30002}
    FjParser := TJSONParser.Create(FTorrentsText.Text, [joUTF8, joIgnoreTrailingComma]);
  {$ELSE}
    FjParser := TJSONParser.Create(FTorrentText.Text, True);
  {$ENDIF}
    try
      FjData := FjParser.Parse;
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
        FjData.Free;
      end;
    finally
      FjParser.Free;
    end;
  finally
    FqBTorrents.Free;
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoadFromStream;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
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

initialization
  RegisterTest(TTestTqBTorrents);
end.

