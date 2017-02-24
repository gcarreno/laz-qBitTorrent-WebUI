{
  Torrent file descriptor and container

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
unit qBTorrents;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, SysUtils;

type
{ TqBTorrentStates }
  TqBTorrentState = (
    tsError,
    tsPausedUp,
    tsPausedDl,
    tsQueuedUp,
    tsQueuedDl,
    tsUploading,
    tsStalledUp,
    tsStalledDl,
    tsCheckingUp,
    tsCheckingDl,
    tsDownloading,
    tsMetaDl,
    tsUnknown
  );

{ TqBTorrentsFilter }
  TqBTorrentsFilter = class(TObject)
  private
    FFilters: TStringList;

    function GetFilters: String;
    function GetCount: Integer;
    procedure Add(const aName:String; const aValue: String);
    procedure Add(const aName:String; const aValue: Boolean);
    procedure Add(const aName:String; const aValue: Integer);
    procedure Remove(const aName: String);
  protected
  public
    constructor Create;
    destructor Destroy; override;

    function Clear: TqBTorrentsFilter;
    function withFilter(const aFilter: String): TqBTorrentsFilter;
    function withOutFilter: TqBTorrentsFilter;
    function withCategory(const aCategory: String): TqBTorrentsFilter;
    function withOutCategory: TqBTorrentsFilter;
    function withSort(const aSort: String): TqBTorrentsFilter;
    function withOutSort: TqBTorrentsFilter;
    function withReverse(const aReverse: Boolean): TqBTorrentsFilter;
    function withOutReverse: TqBTorrentsFilter;
    function withLimit(const aLimit: Integer): TqBTorrentsFilter;
    function withOutLimit: TqBTorrentsFilter;
    function withOffset(const aOffset: Integer): TqBTorrentsFilter;
    function withOutOffset: TqBTorrentsFilter;

    property Filters: String
      read GetFilters;
    property Count: Integer
      read GetCount;
  end;

{ TqBTorrent }
  TqBTorrent = class(TObject)
  private
    FHash: String;
    FName: String;
    FSize: Int64;
    FProgress: Double;
    FDlSpeed: Integer;
    FUpSpeed: Integer;
    FPriority: Integer;
    FNumSeeds: Integer;
    FNumComplete: Integer;
    FNumLeechs: Integer;
    FNumIncomplete: Integer;
    FRatio: Double;
    FEta: Integer; //Seconds
    FState: TqBTorrentState;
    FSeqDl: Boolean;
    FFirstLastPiecePrioritized: Boolean;
    FCategory: String;
    FSuperSeeding: Boolean;
    FForceStart: Boolean;
    FSavePath: String;
    FAddedOn: TDateTime;
    FCompletionOn: TDateTime;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    property Hash: String
      read FHash
      write FHash;
    property Name: String
      read FName
      write FName;
    property Size: Int64
      read FSize
      write FSize;
    property Progress: Double
      read FProgress
      write FProgress;
    property DlSpeed: Integer
      read FDlSpeed
      write FDlSpeed;
    property UpSpeed: Integer
      read FUpSpeed
      write FUpSpeed;
    property Priority: Integer
      read FPriority
      write FPriority;
    property NumSeeds: Integer
      read FNumSeeds
      write FNumSeeds;
    property NumComplete: Integer
      read FNumComplete
      write FNumComplete;
    property NumLeechs: Integer
      read FNumLeechs
      write FNumLeechs;
    property NumIncomplete: Integer
      read FNumIncomplete
      write FNumIncomplete;
    property Ratio: Double
      read FRatio
      write FRatio;
    property Eta: Integer // Seconds
      read FEta
      write FEta;
    property State: TqBTorrentState
      read FState
      write FState;
    property SeqDl: Boolean
      read FSeqDl
      write FSeqDl;
    property FirstLastPiecePrioritized: Boolean
      read FFirstLastPiecePrioritized
      write FFirstLastPiecePrioritized;
    property Category: String
      read FCategory
      write FCategory;
    property SuperSeeding: Boolean
      read FSuperSeeding
      write FSuperSeeding;
    property ForceStart: Boolean
      read FForceStart
      write FForceStart;
    property SavePath: String
      read FSavePath
      write FSavePath;
    property AddedOn: TDateTime
      read FAddedOn
      write FAddedOn;
    property CompletionOn: TDateTime
      read FCompletionOn
      write FCompletionOn;
  end;

{ TqBTorrents }
  TqBTorrents = class(TFPObjectList)
  private
    function GetItem(Index: Integer): TqBTorrent;
    procedure SetItem(Index: Integer; AObject: TqBTorrent);
  protected
  public
    property Items[Index: Integer]: TqBTorrent
      read GetItem
      write SetItem; default;
  end;

// Helper functions
function StrToqBState(const aState: String): TqBTorrentState;
function qBStateToStr(aState: TqBTorrentState): String;

implementation

const
  cNameFilter = 'filter';
  cNameCategory = 'category';
  cNameSort = 'sort';
  cNameReverse = 'reverse';
  cNameLimit = 'limit';
  cNameOffset = 'offset';

function StrToqBState(const aState: String): TqBTorrentState;
begin
  Result := tsUnknown;
  if aState = 'error' then
  begin
    Result := tsError;
    exit;
  end;
  if aState = 'pausedUP' then
  begin
    Result := tsPausedUp;
    exit;
  end;
  if aState = 'pausedDL' then
  begin
    Result := tsPausedDl;
    exit;
  end;
  if aState = 'queuedUP' then
  begin
    Result := tsQueuedUp;
    exit;
  end;
  if aState = 'queuedDL' then
  begin
    Result := tsQueuedDl;
    exit;
  end;
  if aState = 'uploading' then
  begin
    Result := tsUploading;
    exit;
  end;
  if aState = 'stalledUP' then
  begin
    Result := tsStalledUp;
    exit;
  end;
  if aState = 'stalledDL' then
  begin
    Result := tsStalledDl;
    exit;
  end;
  if aState = 'checkingUP' then
  begin
    Result := tsCheckingUp;
    exit;
  end;
  if aState = 'checkingDL' then
  begin
    Result := tsCheckingDl;
    exit;
  end;
  if aState = 'downloading' then
  begin
    Result := tsDownloading;
    exit;
  end;
  if aState = 'metaDL' then
  begin
    Result := tsMetaDl;
    exit;
  end;
end;

function qBStateToStr(aState: TqBTorrentState): String;
begin
  Result := 'unknown';
  case aState of
    tsError:       Result := 'error';
    tsPausedUp:    Result := 'pausedUP';
    tsPausedDl:    Result := 'pausedDL';
    tsQueuedUp:    Result := 'queuedUP';
    tsQueuedDl:    Result := 'queuedDL';
    tsUploading:   Result := 'uploading';
    tsStalledUp:   Result := 'stalledUP';
    tsStalledDl:   Result := 'stalledDL';
    tsCheckingUp:  Result := 'checkingUP';
    tsCheckingDl:  Result := 'checkingDL';
    tsDownloading: Result := 'downloading';
    tsMetaDl:      Result := 'metaDL';
  end;
end;

{ TqBTorrentsFilter }

function TqBTorrentsFilter.GetFilters: String;
begin
  Result := FFilters.DelimitedText;
end;

function TqBTorrentsFilter.GetCount: Integer;
begin
  Result := FFilters.Count;
end;

procedure TqBTorrentsFilter.Add(const aName:String; const aValue: String);
var
  sFilter: String;
  i, index: integer;
begin
  index := -1;
  for i := 0 to FFilters.Count - 1 do
  begin
    if FFilters.Names[i] = aName then
    begin
      index := i;
      break;
    end;
  end;
  if index > -1 then
  begin
    FFilters.ValueFromIndex[index] := aValue;
  end
  else
  begin
    sFilter := aName + '=' + aValue;
    FFilters.Add(sFilter);
  end;
end;

procedure TqBTorrentsFilter.Add(const aName: String; const aValue: Boolean);
var
  sFilter: String;
  i, index: integer;
begin
  index := -1;
  for i := 0 to FFilters.Count - 1 do
  begin
    if FFilters.Names[i] = aName then
    begin
      index := i;
      break;
    end;
  end;
  if index > -1 then
  begin
    if aValue then
    begin
      FFilters.ValueFromIndex[index] := 'true';
    end
    else
    begin
      FFilters.ValueFromIndex[index] := 'false';
    end;
  end
  else
  begin
    if aValue then
    begin
      sFilter := aName + '=true';
    end
    else
    begin
      sFilter := aName + '=false';
    end;
    FFilters.Add(sFilter);
  end;
end;

procedure TqBTorrentsFilter.Add(const aName: String; const aValue: Integer);
var
  sFilter: String;
  i, index: integer;
begin
  index := -1;
  for i := 0 to FFilters.Count - 1 do
  begin
    if FFilters.Names[i] = aName then
    begin
      index := i;
      break;
    end;
  end;
  if index > -1 then
  begin
    FFilters.ValueFromIndex[index] := IntToStr(aValue);
  end
  else
  begin
    sFilter := aName + '=' + IntToStr(aValue);
    FFilters.Add(sFilter);
  end;
end;

procedure TqBTorrentsFilter.Remove(const aName: String);
var
  i, index: Integer;
begin
  index := -1;
  for i := 0 to FFilters.Count - 1 do
  begin
    if FFilters.Names[i] = aName then
    begin
      index := i;
      break;
    end;
  end;
  if index > -1 then
  begin
    FFilters.Delete(index);
  end;
end;

constructor TqBTorrentsFilter.Create;
begin
  FFilters := TStringList.Create;
  FFilters.NameValueSeparator := '=';
  FFilters.AlwaysQuote := False;
  FFilters.Delimiter := '&';
end;

destructor TqBTorrentsFilter.Destroy;
begin
  FFilters.Free;
  inherited Destroy;
end;

function TqBTorrentsFilter.Clear: TqBTorrentsFilter;
begin
  FFilters.Clear;
  Result := Self;
end;

function TqBTorrentsFilter.withFilter(const aFilter: String): TqBTorrentsFilter;
begin
  Add(cNameFilter, aFilter);
  Result := Self;
end;

function TqBTorrentsFilter.withOutFilter: TqBTorrentsFilter;
begin
  Remove(cNameFilter);
  Result := Self;
end;

function TqBTorrentsFilter.withCategory(const aCategory: String): TqBTorrentsFilter;
begin
  Add(cNameCategory, aCategory);
  Result := Self;
end;

function TqBTorrentsFilter.withOutCategory: TqBTorrentsFilter;
begin
  Remove(cNameCategory);
  Result := Self;
end;

function TqBTorrentsFilter.withSort(const aSort: String): TqBTorrentsFilter;
begin
  Add(cNameSort, aSort);
  Result := Self;
end;

function TqBTorrentsFilter.withOutSort: TqBTorrentsFilter;
begin
  Remove(cNameSort);
  Result := Self;
end;

function TqBTorrentsFilter.withReverse(const aReverse: Boolean): TqBTorrentsFilter;
begin
  Add(cNameReverse, aReverse);
  Result := Self;
end;

function TqBTorrentsFilter.withOutReverse: TqBTorrentsFilter;
begin
  Remove(cNameReverse);
  Result := Self;
end;

function TqBTorrentsFilter.withLimit(const aLimit: Integer): TqBTorrentsFilter;
begin
  Add(cNameLimit, aLimit);
  Result := Self;
end;

function TqBTorrentsFilter.withOutLimit: TqBTorrentsFilter;
begin
  Remove(cNameLimit);
  Result := Self;
end;

function TqBTorrentsFilter.withOffset(const aOffset: Integer): TqBTorrentsFilter;
begin
  Add(cNameOffset, aOffset);
  Result := Self;
end;

function TqBTorrentsFilter.withOutOffset: TqBTorrentsFilter;
begin
  Remove(cNameOffset);
  Result := Self;
end;

{ TqBTorrent }

constructor TqBTorrent.Create;
begin
  //
end;

destructor TqBTorrent.Destroy;
begin
  inherited Destroy;
end;

{ TqBTorrents }

function TqBTorrents.GetItem(Index: Integer): TqBTorrent;
begin
  Result := TqBTorrent(inherited GetItem(Index));
end;

procedure TqBTorrents.SetItem(Index: Integer; AObject: TqBTorrent);
begin
  inherited SetItem(Index, AObject);
end;

end.

