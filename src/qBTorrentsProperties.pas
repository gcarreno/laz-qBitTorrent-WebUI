{
  Torrent Generic Properties Container

  Copyright (c) 2017 Gustavo Carreno <guscarreno@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit qBTorrentsProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, fpjson, jsonparser, jsonscanner;

type

{ TqBTorrentsProperties }
  TqBTorrentsProperties = class(TObject)
  private
    FSavePath: String;
    FCreationDate: TDateTime;
    FPieceSize: Integer;
    FComment: String;
    FTotalWasted: Integer;
    FTotalUploaded: Integer;
    FTotalUploadedSession: Integer;
    FTotalDownloaded: Integer;
    FTotalDownloadedSession: Integer;
    FUpLimit: Integer;
    FDlLimit: Integer;
    FTimeElapsed: Integer; // seconds
    FSeedingTime: Integer; // seconds
    FNbConnections: Integer;
    FNbConnectionsLimit: Integer;
    FShareRatio: Double;
    FAdditionDate: TDateTime;
    FCompletionDate: TDateTime;
    FCreatedBy: String;
    FDlSpeed: Integer;
    FDlSpeedAvg: Integer;
    FUpSpeed: Integer;
    FUpSpeedAvg: Integer;
    FEta: Integer; // seconds
    FLastSeen: TDateTime;
    FPeers: Integer;
    FPeersTotal: Integer;
    FPiecesHave: Integer;
    FPiecesNum: Integer;
    FReannounce: Integer;
    FSeeds: Integer;
    FSeedsTotal: Integer;
    FTotalSize: Integer;


    procedure DoLoadFromJSON(const aJSON: String);
    procedure DoLoadFromJSONObj(const aJSONObj: TJSONObject);
    procedure DoLoadFromStream(const aStream: TStream);
  protected
  public
    constructor Create;
    constructor Create(const aJSON: String);
    constructor Create(const aJSONObj: TJSONObject);
    constructor Create(const aStream: TStream);

    destructor Destroy; override;

    procedure Load(const aJSON: String);
    procedure Load(const aJSONObj: TJSONObject);
    procedure Load(const aStream: TStream);

    property SavePath: String
      read FSavePath
      write FSavePath;
    property CreationDate: TDateTime
      read FCreationDate
      write FCreationDate;
    property PieceSize: Integer
      read FPieceSize
      write FPieceSize;
    property Comment: String
      read FComment
      write FComment;
    property TotalWasted: Integer
      read FTotalWasted
      write FTotalWasted;
    property TotalUploaded: Integer
      read FTotalUploaded
      write FTotalUploaded;
    property TotalUploadedSession: Integer
      read FTotalUploadedSession
      write FTotalUploadedSession;
    property TotalDownloaded: Integer
      read FTotalDownloaded
      write FTotalDownloaded;
    property TotalDownloadedSession: Integer
      read FTotalDownloadedSession
      write FTotalDownloadedSession;
    property UpLimit: Integer
      read FUpLimit
      write FUpLimit;
    property DlLimit: Integer
      read FDlLimit
      write FDlLimit;
    property TimeElapsed: Integer
      read FTimeElapsed
      write FTimeElapsed;
    property SeedingTime: Integer
      read FSeedingTime
      write FSeedingTime;
    property NbConnections: Integer
      read FNbConnections
      write FNbConnections;
    property NbConnectionsLimit: Integer
      read FNbConnectionsLimit
      write FNbConnectionsLimit;
    property ShareRatio: Double
      read FShareRatio
      write FShareRatio;
    property AdditionDate: TDateTime
      read FAdditionDate
      write FAdditionDate;
    property CompletionDate: TDateTime
      read FCompletionDate
      write FCompletionDate;
    property CreatedBy: String
      read FCreatedBy
      write FCreatedBy;
    property DlSpeed: Integer
      read FDlSpeed
      write FDlSpeed;
    property DlSpeedAvg: Integer
      read FDlSpeedAvg
      write FDlSpeedAvg;
    property UpSpeed: Integer
      read FUpSpeed
      write FUpSpeed;
    property UpSpeedAvg: Integer
      read FUpSpeedAvg
      write FUpSpeedAvg;
    property Eta: Integer
      read FEta
      write FEta;
    property LastSeen: TDateTime
      read FLastSeen
      write FLastSeen;
    property Peers: Integer
      read FPeers
      write FPeers;
    property PeersTotal: Integer
      read FPeersTotal
      write FPeersTotal;
    property PiecesHave: Integer
      read FPiecesHave
      write FPiecesHave;
    property PiecesNum: Integer
      read FPiecesNum
      write FPiecesNum;
    property Reannounce: Integer
      read FReannounce
      write FReannounce;
    property Seeds: Integer
      read FSeeds
      write FSeeds;
    property SeedsTotal: Integer
      read FSeedsTotal
      write FSeedsTotal;
    property TotalSize: Integer
      read FTotalSize
      write FTotalSize;
  end;

implementation

{ TqBTorrentsProperties }

constructor TqBTorrentsProperties.Create;
begin
  FSavePath := '';
  FCreationDate := 0.0;
  FPieceSize := -1;
  FComment := '';
  FTotalWasted := -1;
  FTotalUploaded := -1;
  FTotalUploadedSession := -1;
  FTotalDownloaded := -1;
  FTotalDownloadedSession := -1;
  FUpLimit := -1;
  FDlLimit := -1;
  FTimeElapsed := -1;
  FSeedingTime := -1;
  FNbConnections := -1;
  FNbConnectionsLimit := -1;
  FShareRatio := 0.0;
  FAdditionDate := 0.0;
  FCompletionDate := 0.0;
  FCreatedBy := '';
  FDlSpeed := -1;
  FDlSpeedAvg := -1;
  FUpSpeed := -1;
  FUpSpeedAvg := -1;
  FEta := -1;
  FLastSeen := 0.0;
  FPeers := -1;
  FPeersTotal := -1;
  FPiecesHave := -1;
  FPiecesNum := -1;
  FReannounce := -1;
  FSeeds := -1;
  FSeedsTotal := -1;
  FTotalSize := -1;
end;

constructor TqBTorrentsProperties.Create(const aJSON: String);
begin
  Create;
  DoLoadFromJSON(aJSON);
end;

constructor TqBTorrentsProperties.Create(const aJSONObj: TJSONObject);
begin
  Create;
  DoLoadFromJSONObj(aJSONObj);
end;

constructor TqBTorrentsProperties.Create(const aStream: TStream);
begin
  Create;
  DoLoadFromStream(aStream);
end;

destructor TqBTorrentsProperties.Destroy;
begin
  inherited Destroy;
end;

procedure TqBTorrentsProperties.DoLoadFromJSON(const aJSON: String);
var
  jParser: TJSONParser;
  jData: TJSONData;
begin
  jParser := TJSONParser.Create(aJSON, [joUTF8, joIgnoreTrailingComma]);
  try
    jData := jParser.Parse;
    try
      if jData.JSONType = jtObject then
      begin
        Load(jData as TJSONObject);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrentsProperties.Load(const aJSON: String);
begin
  DoLoadFromJSON(aJSON);
end;

procedure TqBTorrentsProperties.DoLoadFromJSONObj(const aJSONObj: TJSONObject);
var
  iUnixTime: Integer;
  dtTime: TDateTime;
begin
  FSavePath := aJSONObj.Get('save_path', FSavePath);

  iUnixTime := aJSONObj.Get('save_path', DateTimeToUnix(FCreationDate));
  dtTime := UnixToDateTime(iUnixTime);
  if (FCreationDate <> dtTime) and (iUnixTime > 0) then
  begin
   FCreationDate := dtTime;
  end;

  FPieceSize := aJSONObj.Get('piece_size', FPieceSize);
  FComment := aJSONObj.Get('comment', FComment);
  FTotalWasted := aJSONObj.Get('total_wasted', FTotalWasted);
  FTotalUploaded := aJSONObj.Get('total_uploaded', FTotalUploaded);
  FTotalUploadedSession := aJSONObj.Get('total_uploaded_session', FTotalUploadedSession);
  FTotalDownloaded := aJSONObj.Get('total_downloaded', FTotalDownloaded);
  FTotalDownloadedSession := aJSONObj.Get('total_downloaded_session', FTotalDownloadedSession);
  FUpLimit := aJSONObj.Get('up_limit', FUpLimit);
  FDlLimit := aJSONObj.Get('dl_limit', FDlLimit);
  FTimeElapsed := aJSONObj.Get('time_elapsed', FTimeElapsed);
  FSeedingTime := aJSONObj.Get('seeding_time', FSeedingTime);
  FNbConnections := aJSONObj.Get('nb_connections', FNbConnections);
  FNbConnectionsLimit := aJSONObj.Get('nb_connections_limit', FNbConnectionsLimit);
  FShareRatio := aJSONObj.Get('share_ratio', FShareRatio);

  iUnixTime := aJSONObj.Get('addition_date', DateTimeToUnix(FAdditionDate));
  dtTime := UnixToDateTime(iUnixTime);
  if (FAdditionDate <> dtTime) and (iUnixTime > 0) then
  begin
    FAdditionDate := dtTime;
  end;

  iUnixTime := aJSONObj.Get('completion_date', DateTimeToUnix(FCompletionDate));
  dtTime := UnixToDateTime(iUnixTime);
  if (FCompletionDate <> dtTime) and (iUnixTime > 0) then
  begin
   FCompletionDate := dtTime;
  end;

  FCreatedBy := aJSONObj.Get('created_by', FCreatedBy);
  FDlSpeed := aJSONObj.Get('dl_speed', FDlSpeed);
  FDlSpeedAvg := aJSONObj.Get('dl_speed_avg', FDlSpeedAvg);
  FUpSpeed := aJSONObj.Get('up_speed', FUpSpeed);
  FUpSpeedAvg := aJSONObj.Get('up_speed_avg', FUpSpeedAvg);
  FEta := aJSONObj.Get('eta', FEta);

  iUnixTime := aJSONObj.Get('last_seen', DateTimeToUnix(FLastSeen));
  dtTime := UnixToDateTime(iUnixTime);
  if (FLastSeen <> dtTime) and (iUnixTime > 0) then
  begin
    FLastSeen := dtTime;
  end;

  FPeers := aJSONObj.Get('peers', FPeers);
  FPeersTotal := aJSONObj.Get('peers_total', FPeersTotal);
  FPiecesHave := aJSONObj.Get('pieces_have', FPiecesHave);
  FPiecesNum := aJSONObj.Get('pieces_num', FPiecesNum);
  FReannounce := aJSONObj.Get('reannounce', FReannounce);
  FSeeds := aJSONObj.Get('seeds', FSeeds);
  FSeedsTotal := aJSONObj.Get('seeds_total', FSeedsTotal);
  FTotalSize := aJSONObj.Get('total_size', FTotalSize);
end;

procedure TqBTorrentsProperties.Load(const aJSONObj: TJSONObject);
begin
  DoLoadFromJSONObj(aJSONObj);
end;

procedure TqBTorrentsProperties.DoLoadFromStream(const aStream: TStream);
var
  jParser: TJSONParser;
  jData: TJSONData;
begin
  jParser := TJSONParser.Create(aStream, [joUTF8, joIgnoreTrailingComma]);
  try
    jData := jParser.Parse;
    try
      if jData.JSONType = jtObject then
      begin
        Load(jData as TJSONObject);
      end;
    finally
      jData.Free;
    end;
  finally
    jParser.Free;
  end;
end;

procedure TqBTorrentsProperties.Load(const aStream: TStream);
begin
  DoLoadFromStream(aStream);
end;

end.
