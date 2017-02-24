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
  protected
  public
    constructor Create;
    destructor Destroy; override;

    function Clear: TqBTorrentsFilter;
    function withFilter(const aFilter: String): TqBTorrentsFilter;
    function withOutFilter: TqBTorrentsFilter;
    function withCategory(const aFilter: String): TqBTorrentsFilter;
    function withOutCategory: TqBTorrentsFilter;
    function withSort(const aFilter: String): TqBTorrentsFilter;
    function withOutSort: TqBTorrentsFilter;
    function withReverse(const aFilter: Boolean): TqBTorrentsFilter;
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

constructor TqBTorrentsFilter.Create;
begin
  FFilters := TStringList.Create;
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
  Result := Self;
end;

function TqBTorrentsFilter.withOutFilter: TqBTorrentsFilter;
begin
  Result := Self;
end;

function TqBTorrentsFilter.withCategory(const aFilter: String
  ): TqBTorrentsFilter;
begin
  Result := Self;
end;

function TqBTorrentsFilter.withOutCategory: TqBTorrentsFilter;
begin
  Result := Self;
end;

function TqBTorrentsFilter.withSort(const aFilter: String): TqBTorrentsFilter;
begin
  Result := Self;
end;

function TqBTorrentsFilter.withOutSort: TqBTorrentsFilter;
begin
  Result := Self;
end;

function TqBTorrentsFilter.withReverse(const aFilter: Boolean
  ): TqBTorrentsFilter;
begin
  Result := Self;
end;

function TqBTorrentsFilter.withOutReverse: TqBTorrentsFilter;
begin
  Result := Self;
end;

function TqBTorrentsFilter.withLimit(const aLimit: Integer): TqBTorrentsFilter;
begin
  Result := Self;
end;

function TqBTorrentsFilter.withOutLimit: TqBTorrentsFilter;
begin
  Result := Self;
end;

function TqBTorrentsFilter.withOffset(const aOffset: Integer): TqBTorrentsFilter;
begin
  Result := Self;
end;

function TqBTorrentsFilter.withOutOffset: TqBTorrentsFilter;
begin
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

