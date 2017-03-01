{
  Utility functions and constants for laz-qBitTorrent-WebUI

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
unit qBUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, qBTorrents, qBTorrentsTrackers;

function FormatBytes(aSize: Int64): String;
function FormatBytesPerSecond(aSize: Int64): String;
function FormatTorrentState(aState: TqBTorrentState): String;
function FormatTrackerStatus(aStatus: TqBTrackerStatus): String;

implementation

const
  ciKilo: Int64 = 1024;
  ciMega: Int64 = 1048576;
  ciGiga: Int64 = 1073741824;
  ciTera: Int64 = 1099511627776;
  ciPeta: Int64 = 1125899906842624;

  csByte = 'B';
  csKilo = 'KB';
  csMega = 'MB';
  csGiga = 'GB';
  csTera = 'TB';
  csPeta = 'PB';

function FormatBytes(aSize: Int64): String;
var
  dSize: Double;
begin
  Result := '';
  dSize := 0.0;
  if aSize < ciKilo then
  begin
    Result := IntToStr(aSize) + csByte;
    exit;
  end;
  if aSize < ciMega then
  begin
    dSize := aSize / ciKilo;
    Result := FormatFloat('0.##'+csKilo, dSize);
    exit;
  end;
  if aSize < ciGiga then
  begin
    dSize := aSize / ciMega;
    Result := FormatFloat('0.##'+csMega, dSize);
    exit;
  end;
  if aSize < ciTera then
  begin
    dSize := aSize / ciGiga;
    Result := FormatFloat('0.##'+csGiga, dSize);
    exit;
  end;
  if aSize < ciPeta then
  begin
    dSize := aSize / ciTera;
    Result := FormatFloat('0.##'+csTera, dSize);
    exit;
  end;
  dSize := aSize / ciPeta;
  Result := FormatFloat('0.##'+csPeta, dSize);
end;

function FormatBytesPerSecond(aSize: Int64): String;
begin
  Result := FormatBytes(aSize) + '/s';
end;

function FormatTorrentState(aState: TqBTorrentState): String;
begin
  Result := 'Unknown';
  case aState of
    qtsError:       Result := 'Error';
    qtsPausedUp:    Result := 'Paused Upload';
    qtsPausedDl:    Result := 'Paused Download';
    qtsQueuedUp:    Result := 'Queued Upload';
    qtsQueuedDl:    Result := 'Queued Download';
    qtsUploading:   Result := 'Uploading';
    qtsStalledUp:   Result := 'Stalled Upload';
    qtsStalledDl:   Result := 'Stalled Download';
    qtsCheckingUp:  Result := 'Checking Upload';
    qtsCheckingDl:  Result := 'Checking Download';
    qtsDownloading: Result := 'Downloading';
    qtsMetaDl:      Result := 'Downloading Metadata';
  end;
end;

function FormatTrackerStatus(aStatus: TqBTrackerStatus): String;
begin
  Result := qBTrackerStatusToStr(aStatus);
end;

end.
