unit TestTqBTorrent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson,
  jsonparser, jsonscanner,
  qBTorrents;

type

  { TTestTqBTorrent }

  TTestTqBTorrent = class(TTestCase)
  private
    FqBTorrent: TqBTorrent;

    FTorrentText: TStringList;
    FTorrentStream: TFileStream;
    FjParser: TJSONParser;
    FjData: TJSONData;

    procedure TestTorrent1Fields;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Create Torrent
    procedure TestTorrentCreateFromJSON;
    procedure TestTorrentCreateFromJSONObject;
  end;

implementation

procedure TTestTqBTorrent.TestTorrent1Fields;
begin
  { TODO 10 -ogcarreno -cTqBTorrent : Finish the filed list }
  AssertEquals('Torrent Hash', '0403fb4728bd788fbcb67e87d6feb241ef38c75a', FqBTorrent.Hash);
  AssertEquals('Torrent Name', 'ubuntu-16.10-desktop-amd64.iso', FqBTorrent.Name);
  AssertEquals('Torrent Size', 1593835520, FqBTorrent.Size);
  AssertEquals('Torrent Progress', 0.0, FqBTorrent.Progress);
  AssertEquals('Torrent Dl Speed', 0, FqBTorrent.DlSpeed);
  AssertEquals('Torrent Added On', StrToDateTime('5-3-17 22:56:05'), FqBTorrent.AddedOn);
end;

procedure TTestTqBTorrent.SetUp;
var
  sFileName: String;
begin
  sFileName := ExtractFileDir(ParamStr(0));
  sFileName := sFileName + '/../tests/data/torrent-1.json';
  FTorrentText := TStringList.Create;
  FTorrentText.LoadFromFile(sFileName);
  FTorrentStream := TFileStream.Create(sFileName, fmOpenRead);
  //inherited SetUp;
end;

procedure TTestTqBTorrent.TearDown;
begin
  FTorrentText.Free;
  FTorrentStream.Free;
  //inherited TearDown;
end;

procedure TTestTqBTorrent.TestTorrentCreateFromJSON;
begin
  try
    FqBTorrent := TqBTorrent.Create(FTorrentText.Text);
    TestTorrent1Fields;
  finally
    FqBTorrent.Free;
  end;
end;

procedure TTestTqBTorrent.TestTorrentCreateFromJSONObject;
begin
  {$IF FPC_FULLVERSION >= 30002}
    FjParser := TJSONParser.Create(FTorrentText.Text, [joUTF8, joIgnoreTrailingComma]);
  {$ELSE}
    FjParser := TJSONParser.Create(FTorrentText.Text, True);
  {$ENDIF}
  try
    FjData := FjParser.Parse;
    try
      try
        FqBTorrent := TqBTorrent.Create(FjData as TJSONObject);
        TestTorrent1Fields;
      finally
        FqBTorrent.Free;
      end;
    finally
      if Assigned(FjData) then
        FjData.Free;
    end;
  finally
    FjParser.Free;
  end;
end;

initialization
  RegisterTest(TTestTqBTorrent);
end.

