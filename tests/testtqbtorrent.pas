unit TestTqBTorrent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, fpjson,
  jsonparser, jsonscanner,
  qBTorrents;

type

  { TTestTqBTorrent }

  TTestTqBTorrent = class(TTestCase)
  private
    FqBTorrent: TqBTorrent;

    procedure TestTorrent1Fields;
  published
    // Create Torrent
    procedure TestTorrentCreateFromJSON;
    procedure TestTorrentCreateFromJSONObject;
  end;

implementation

procedure TTestTqBTorrent.TestTorrent1Fields;
begin
  AssertEquals('Torrent Hash', '0403fb4728bd788fbcb67e87d6feb241ef38c75a', FqBTorrent.Hash);
  AssertEquals('Torrent Name', 'ubuntu-16.10-desktop-amd64.iso', FqBTorrent.Name);
  AssertEquals('Torrent Size', 1593835520, FqBTorrent.Size);
  AssertEquals('Torrent Progress', 0.0, FqBTorrent.Progress);
  AssertEquals('Torrent Added On', StrToDateTime('5-3-17 22:56:05'), FqBTorrent.AddedOn);
end;

procedure TTestTqBTorrent.TestTorrentCreateFromJSON;
var
  sFileName: String;
  slJSON: TStringList;
begin
  sFileName := ExtractFileDir(ParamStr(0));
  sFileName := sFileName + '/../tests/data/torrent-1.json';
  slJSON := TStringList.Create;
  try
    slJSON.LoadFromFile(sFileName);
    try
      FqBTorrent := TqBTorrent.Create(slJSON.Text);
      TestTorrent1Fields;
    finally
      FqBTorrent.Free;
    end;
  finally
    slJSON.Free;
  end;
end;

procedure TTestTqBTorrent.TestTorrentCreateFromJSONObject;
var
  sFileName: String;
  slJSON: TStringList;
  jParser: TJSONParser;
  jData: TJSONData;
begin
  sFileName := ExtractFileDir(ParamStr(0));
  sFileName := sFileName + '/../tests/data/torrent-1.json';
  slJSON := TStringList.Create;
  try
    slJSON.LoadFromFile(sFileName);
    {$IF FPC_FULLVERSION >= 30002}
      jParser := TJSONParser.Create(slJSON.Text, [joUTF8, joIgnoreTrailingComma]);
    {$ELSE}
      jParser := TJSONParser.Create(slJSON.Text, True);
    {$ENDIF}
    try
      jData := jParser.Parse;
      try
        try
          FqBTorrent := TqBTorrent.Create(jData as TJSONObject);
          TestTorrent1Fields;
        finally
          FqBTorrent.Free;
        end;
      finally
        jData.Free;
      end;
    finally
      jParser.Free;
    end;
  finally
    slJSON.Free;
  end;
end;

initialization
  RegisterTest(TTestTqBTorrent);
end.

