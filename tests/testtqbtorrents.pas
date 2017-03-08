unit TestTqBTorrents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, fpjson, jsonparser,
  jsonscanner, qBTorrents;

type

  { TTestTqBTorrents }

  TTestTqBTorrents = class(TTestCase)
  private
    FqBTorrents: TqBTorrents;
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
var
  sFileName: String;
  slJSON: TStringList;
begin
  sFileName := ExtractFileDir(ParamStr(0));
  sFileName := sFileName + '/../tests/data/torrents.json';
  slJSON := TStringList.Create;
  try
    slJSON.LoadFromFile(sFileName);
    FqBTorrents := TqBTorrents.Create(True);
    try
      FqBTorrents.LoadTorrents(slJSON.Text);
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
  finally
    slJSON.Free;
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoadFromJSONData;
var
  sFileName: String;
  slJSON: TStringList;
  jParser: TJSONParser;
  jData: TJSONData;
begin
  sFileName := ExtractFileDir(ParamStr(0));
  sFileName := sFileName + '/../tests/data/torrents.json';
  slJSON := TStringList.Create;
  try
    slJSON.LoadFromFile(sFileName);
    FqBTorrents := TqBTorrents.Create(True);
    try
    {$IF FPC_FULLVERSION >= 30002}
      jParser := TJSONParser.Create(slJSON.Text, [joUTF8, joIgnoreTrailingComma]);
    {$ELSE}
      jParser := TJSONParser.Create(slJSON.Text, True);
    {$ENDIF}
      try
        jData := jParser.Parse;
        try
          FqBTorrents.LoadTorrents(jData);
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
          jData.Free;
        end;
      finally
        jParser.Free;
      end;
    finally
      FqBTorrents.Free;
    end;
  finally
    slJSON.Free;
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoadFromJSONArray;
var
  sFileName: String;
  slJSON: TStringList;
  jParser: TJSONParser;
  jData: TJSONData;
begin
  sFileName := ExtractFileDir(ParamStr(0));
  sFileName := sFileName + '/../tests/data/torrents.json';
  slJSON := TStringList.Create;
  try
    slJSON.LoadFromFile(sFileName);
    FqBTorrents := TqBTorrents.Create(True);
    try
    {$IF FPC_FULLVERSION >= 30002}
      jParser := TJSONParser.Create(slJSON.Text, [joUTF8, joIgnoreTrailingComma]);
    {$ELSE}
      jParser := TJSONParser.Create(slJSON.Text, True);
    {$ENDIF}
      try
        jData := jParser.Parse;
        try
          FqBTorrents.LoadTorrents(jData as TJSONArray);
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
          jData.Free;
        end;
      finally
        jParser.Free;
      end;
    finally
      FqBTorrents.Free;
    end;
  finally
    slJSON.Free;
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoadFromStream;
var
  sFileName: String;
  stFile: TFileStream;
begin
  sFileName := ExtractFileDir(ParamStr(0));
  sFileName := sFileName + '/../tests/data/torrents.json';
  stFile := TFileStream.Create(sFileName, fmOpenRead);
  try
    FqBTorrents := TqBTorrents.Create(True);
    try
      FqBTorrents.LoadTorrents(stFile);
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
  finally
    stFile.Free;
  end;
end;

initialization
  RegisterTest(TTestTqBTorrents);
end.

