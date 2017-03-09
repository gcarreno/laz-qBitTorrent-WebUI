program lazqbtest;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner,
  TestqButils,
  TestTqBTorrents,
  TestTqBTorrent,
  qBTorrents, qBTorrentsFiles, qBTorrentsProperties, qBTorrentsTrackers,
  qBTorrentsWebSeeds, qBUtils;

type

  { TTestqBitTorrentWebUIRunner }

  TTestqBitTorrentWebUIRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TTestqBitTorrentWebUIRunner;

begin
  Application := TTestqBitTorrentWebUIRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
