{
  Torrent Trackers Container

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
unit qBTorrentsTrackers;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, SysUtils, DateUtils, fpjson, qBCommon;

type

{ TqBTrackerStatus }
  TqBTrackerStatus = (
    qtrsWorking,
    qtrsUpdating,
    qtrsNotWorking,
    qtrsNotContactedYet,
    qtrsUnknown
  );

{ TqBTorrentsTracker }
  TqBTorrentsTracker = class(TObject)
  private
    FUrl: String;
    FStatus: TqBTrackerStatus;
    FNumPeers: Integer;
    FMsg: String;

    procedure DoLoadFromJSON(const aJSON: String);
    procedure DoLoadFromJSONData(const aJSONData: TJSONData);
    procedure DoLoadFromJSONObj(const aJSONObj: TJSONObject);
    procedure DoLoadFromStream(const aStream: TStream);
  protected
  public
    constructor Create;
    constructor Create(const aJSON: String);
    constructor Create(const aJSONData: TJSONData);
    constructor Create(const aJSONObj: TJSONObject);
    constructor Create(const aStream: TStream);

    destructor Destroy; override;

    procedure Load(const aJSON: String);
    procedure Load(const aJSONData: TJSONData);
    procedure Load(const aJSONObj: TJSONObject);
    procedure Load(const aStream: TStream);

    property Url: String
      read FUrl
      write FUrl;
    property Status: TqBTrackerStatus
      read FStatus
      write FStatus;
    property NumPeers: Integer
      read FNumPeers
      write FNumPeers;
    property Msg: String
      read FMsg
      write FMsg;
  end;

{ TqBTorrentsTrackers }
  TqBTorrentsTrackersEnumerator = class; //Forward
  TqBTorrentsTrackers = class(TFPObjectList)
  private
    function GetByIndex(Index: Integer): TqBTorrentsTracker;
    function GetByUrl(Url: String): TqBTorrentsTracker;
    procedure SetByIndex(Index: Integer; AValue: TqBTorrentsTracker);
    procedure SetByUrl(Url: String; AValue: TqBTorrentsTracker);
  protected
  public
    // Enumerator
    function GetEnumerator: TqBTorrentsTrackersEnumerator;

    procedure LoadTrackers(const aJSON: String);
    procedure LoadTrackers(const aJSONData: TJSONData);
    procedure LoadTrackers(const aJSONArray: TJSONArray);
    procedure LoadTrackers(const aStream: TStream);

    procedure UpdateTrackers(const aJSON: String);
    procedure UpdateTrackers(const aJSONData: TJSONData);
    procedure UpdateTrackers(const aJSONArray: TJSONArray);
    procedure UpdateTrackers(const aStream: TStream);

    procedure UpdateTracker(const aUrl: String; const aJSON: String);
    procedure UpdateTracker(const aUrl: String; const aJSONData: TJSONData);
    procedure UpdateTracker(const aUrl: String; const aJSONObj: TJSONObject);
    procedure UpdateTracker(const aUrl: String; const aStream: TStream);

    property Items[Index: Integer]: TqBTorrentsTracker
      read GetByIndex
      write SetByIndex; default;
    property Urls[Url: String]: TqBTorrentsTracker
      read GetByUrl
      write SetByUrl;
  end;

{ TqBTorrentsTrackersEnumerator }
  TqBTorrentsTrackersEnumerator = class(TObject)
  private
    FTrackers: TqBTorrentsTrackers;
    FPosition: Integer;
  public
    constructor Create(ATrackers: TqBTorrentsTrackers);
    function GetCurrent: TqBTorrentsTracker;
    function MoveNext: Boolean;

    property Current: TqBTorrentsTracker
      read GetCurrent;
  end;

// Helper functions
function StrToqBTrackerStatus(const aStatus: String): TqBTrackerStatus;
function qBTrackerStatusToStr(aStatus: TqBTrackerStatus): String;

implementation

const
  csStatusWorking = 'Working';
  csStatusUpdating = 'Updating...';
  csStatusNotWorking = 'Not working';
  csStatusNotContactedYet = 'Not contacted yet';
  csStatusUnknown = 'Unknown';

// Helper functions

function StrToqBTrackerStatus(const aStatus: String): TqBTrackerStatus;
begin
  Result := qtrsUnknown;
  if aStatus = csStatusWorking then
  begin
    Result := qtrsWorking;
    exit;
  end;
  if aStatus = csStatusUpdating then
  begin
    Result := qtrsUpdating;
    exit;
  end;
  if aStatus = csStatusNotWorking then
  begin
    Result := qtrsNotWorking;
    exit;
  end;
  if aStatus = csStatusNotContactedYet then
  begin
    Result := qtrsNotContactedYet;
    exit;
  end;
end;

function qBTrackerStatusToStr(aStatus: TqBTrackerStatus): String;
begin
  Result := csStatusUnknown;
  case aStatus of
    qtrsWorking: Result := csStatusWorking;
    qtrsUpdating: Result := csStatusUpdating;
    qtrsNotWorking: Result := csStatusNotWorking;
    qtrsNotContactedYet: Result := csStatusNotContactedYet;
  end;
end;

{ TqBTorrentsTrackersEnumerator }

constructor TqBTorrentsTrackersEnumerator.Create(ATrackers: TqBTorrentsTrackers);
begin
  FTrackers := ATrackers;
  FPosition := -1;
end;

function TqBTorrentsTrackersEnumerator.GetCurrent: TqBTorrentsTracker;
begin
  Result := FTrackers[FPosition];
end;

function TqBTorrentsTrackersEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FTrackers.Count;
end;

{ TqBTorrentsTracker }

procedure TqBTorrentsTracker.DoLoadFromJSON(const aJSON: String);
var
  jData: TJSONData;
begin
  jData := GetJSONData(aJSON);
  try
    if jData.JSONType = jtObject then
    begin
      DoLoadFromJSONObj(jData as TJSONObject);
    end;
  finally
    jData.Free;
  end;
end;

procedure TqBTorrentsTracker.DoLoadFromJSONData(const aJSONData: TJSONData);
begin
  if aJSONData.JSONType = jtObject then
  begin
    DoLoadFromJSONObj(aJSONData as TJSONObject);
  end;
end;

procedure TqBTorrentsTracker.DoLoadFromJSONObj(const aJSONObj: TJSONObject);
const
  csTrackersUrl = 'url';
  csTrackersStatus = 'status';
  csTrackersNumPeers = 'num_peers';
  csTrackersMsg = 'msg';
begin
  FUrl := aJSONObj.Get(csTrackersUrl, FUrl);
  FStatus := StrToqBTrackerStatus(
    aJSONObj.Get(csTrackersStatus, qBTrackerStatusToStr(FStatus))
  );
  FNumPeers := aJSONObj.Get(csTrackersNumPeers, FNumPeers);
  FMsg := aJSONObj.Get(csTrackersMsg, FMsg);
end;

procedure TqBTorrentsTracker.DoLoadFromStream(const aStream: TStream);
var
  jData: TJSONData;
begin
  jData := GetJSONData(aStream);
  try
    if jData.JSONType = jtObject then
    begin
      DoLoadFromJSONObj(jData as TJSONObject);
    end;
  finally
    jData.Free;
  end;
end;

constructor TqBTorrentsTracker.Create;
begin
  FUrl := '';
  FStatus := qtrsUnknown;
  FNumPeers := -1;
  FMsg := '';
end;

constructor TqBTorrentsTracker.Create(const aJSON: String);
begin
  Create;
  DoLoadFromJSON(aJSON);
end;

constructor TqBTorrentsTracker.Create(const aJSONData: TJSONData);
begin
  Create;
  DoLoadFromJSONData(aJSONData);
end;

constructor TqBTorrentsTracker.Create(const aJSONObj: TJSONObject);
begin
  Create;
  DoLoadFromJSONObj(aJSONObj);
end;

constructor TqBTorrentsTracker.Create(const aStream: TStream);
begin
  Create;
  DoLoadFromStream(aStream);
end;

destructor TqBTorrentsTracker.Destroy;
begin
  inherited Destroy;
end;

procedure TqBTorrentsTracker.Load(const aJSON: String);
begin
  DoLoadFromJSON(aJSON);
end;

procedure TqBTorrentsTracker.Load(const aJSONData: TJSONData);
begin
  DoLoadFromJSONData(aJSONData);
end;

procedure TqBTorrentsTracker.Load(const aJSONObj: TJSONObject);
begin
  DoLoadFromJSONObj(aJSONObj);
end;

procedure TqBTorrentsTracker.Load(const aStream: TStream);
begin
  DoLoadFromStream(aStream);
end;

{ TqBTorrentsTrackers }

function TqBTorrentsTrackers.GetByIndex(Index: Integer): TqBTorrentsTracker;
begin
  Result := inherited Items[Index] as TqBTorrentsTracker;
end;

procedure TqBTorrentsTrackers.SetByIndex(Index: Integer; AValue: TqBTorrentsTracker);
begin
  inherited Items[Index] := AValue;
end;

function TqBTorrentsTrackers.GetByUrl(Url: String): TqBTorrentsTracker;
var
  oTracker: TqBTorrentsTracker;
begin
  Result := nil;
  for oTracker in Self do
  begin
    if oTracker.Url = Url then
    begin
      Result := oTracker;
      break;
    end;
  end;
end;

procedure TqBTorrentsTrackers.SetByUrl(Url: String; AValue: TqBTorrentsTracker);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    if Items[index].Url = Url then
    begin
      inherited Items[index] := aValue;
      break;
    end;
  end;
end;

function TqBTorrentsTrackers.GetEnumerator: TqBTorrentsTrackersEnumerator;
begin
  Result := TqBTorrentsTrackersEnumerator.Create(Self);
end;

procedure TqBTorrentsTrackers.LoadTrackers(const aJSON: String);
var
  jData: TJSONData;
begin
  jData := GetJSONData(aJSON);
  try
    if jData.JSONType = jtArray then
    begin
      LoadTrackers(jData as TJSONArray);
    end;
  finally
    jData.Free;
  end;
end;

procedure TqBTorrentsTrackers.LoadTrackers(const aJSONData: TJSONData);
begin
  if aJSONData.JSONType = jtArray then
  begin
    LoadTrackers(aJSONData as TJSONArray);
  end;
end;

procedure TqBTorrentsTrackers.LoadTrackers(const aJSONArray: TJSONArray);
var
  index: Integer;
begin
  Clear;
  for index := 0 to aJSONArray.Count - 1 do
  begin
    if aJSONArray[index].JSONType = jtObject then
      Add(TqBTorrentsTracker.Create(aJSONArray[index] as TJSONObject));
  end;
end;

procedure TqBTorrentsTrackers.LoadTrackers(const aStream: TStream);
var
  jData: TJSONData;
begin
  jData := GetJSONData(aStream);
  try
    if jData.JSONType = jtArray then
    begin
      LoadTrackers(jData as TJSONArray);
    end;
  finally
    jData.Free;
  end;
end;

procedure TqBTorrentsTrackers.UpdateTrackers(const aJSON: String);
var
  jData: TJSONData;
begin
  jData := GetJSONData(aJSON);
  try
    if jData.JSONType = jtArray then
    begin
      UpdateTrackers(jData as TJSONArray);
    end;
  finally
    jData.Free;
  end;
end;

procedure TqBTorrentsTrackers.UpdateTrackers(const aJSONData: TJSONData);
begin
  if aJSONData.JSONType = jtArray then
  begin
    UpdateTrackers(aJSONData as TJSONArray);
  end;
end;

procedure TqBTorrentsTrackers.UpdateTrackers(const aJSONArray: TJSONArray);
var
  index, index1: Integer;
  oTracker: TqBTorrentsTracker;
  jData: TJSONData;
begin
  for index := 0 to aJSONArray.Count - 1 do
  begin
    jData := aJSONArray[index];
    if jData.JSONType = jtObject then
    begin
      for index1 := 0 to Count - 1 do
      begin
        oTracker := Items[index1];
        if oTracker.Url = TJSONObject(jData).Get('url', '') then
        begin
          break;
        end
        else
        begin
          oTracker:= nil;
        end;
      end;
      if Assigned(oTracker) then
      begin
        oTracker.Load(jData as TJSONObject);
      end
      else
      begin
        Add(TqBTorrentsTracker.Create(jData as TJSONObject));
      end;
    end;
  end;
end;

procedure TqBTorrentsTrackers.UpdateTrackers(const aStream: TStream);
var
  jData: TJSONData;
begin
  jData := GetJSONData(aStream);
  try
    if jData.JSONType = jtArray then
    begin
      UpdateTrackers(jData as TJSONArray);
    end;
  finally
    jData.Free;
  end;
end;

procedure TqBTorrentsTrackers.UpdateTracker(const aUrl: String; const aJSON: String);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    if Items[index].Url = aUrl then
    begin
      Items[index].Load(aJSON);
      break;
    end;
  end;
end;

procedure TqBTorrentsTrackers.UpdateTracker(const aUrl: String; const aJSONData: TJSONData);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    if Items[index].Url = aUrl then
    begin
      Items[index].Load(aJSONData);
      break;
    end;
  end;
end;

procedure TqBTorrentsTrackers.UpdateTracker(const aUrl: String; const aJSONObj: TJSONObject);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    if Items[index].Url = aUrl then
    begin
      Items[index].Load(aJSONObj);
      break;
    end;
  end;
end;

procedure TqBTorrentsTrackers.UpdateTracker(const aUrl: String; const aStream: TStream);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    if Items[index].Url = aUrl then
    begin
      Items[index].Load(aStream);
      break;
    end;
  end;
end;

end.
