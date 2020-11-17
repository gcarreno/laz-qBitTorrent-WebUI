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
  SysUtils, qBTorrents, qBTorrentsTrackers, qBTorrentsFiles;

function FormatBytes(aSize: Int64): String;
function FormatBytesPerSecond(aSize: Int64): String;
function FormatTorrentState(aState: TqBTorrentState): String;
function FormatTrackerStatus(aStatus: TqBTrackerStatus): String;
function FormatFilesPriority(aPriority: Integer): String;

implementation

const
  ciKilo: Int64 = 1024;
  ciMega: Int64 = 1048576;
  ciGiga: Int64 = 1073741824;
  ciTera: Int64 = 1099511627776;
  ciPeta: Int64 = 1125899906842624;

  csFormatFloat = '#,0.##';

  csByte = 'B';
  csKilo = 'KB';
  csMega = 'MB';
  csGiga = 'GB';
  csTera = 'TB';
  csPeta = 'PB';

  csStateError       = 'Error';
  csStatePausedUp    = 'Paused Upload';
  csStatePausedDl    = 'Paused Download';
  csStateQueuedUp    = 'Queued Upload';
  csStateQueuedDl    = 'Queued Download';
  csStateUploading   = 'Uploading';
  csStateStalledUp   = 'Stalled Upload';
  csStateStalledDl   = 'Stalled Download';
  csStateCheckingUp  = 'Checking Upload';
  csStateCheckingDl  = 'Checking Download';
  csStateDownloading = 'Downloading';
  csStateMetaDl      = 'Downloading Metadata';
  csStateUnknown     = 'Unknown';

  function FormatBytes(aSize: Int64): String;
var
  dSize: Double;
begin
  Result := '';
  dSize := 0.0;
  if aSize < ciKilo then
  begin
    Result := FormatFloat(csFormatFloat, aSize) + csByte;
    exit;
  end;
  if aSize < ciMega then
  begin
    dSize := aSize / ciKilo;
    Result := FormatFloat(csFormatFloat+csKilo, dSize);
    exit;
  end;
  if aSize < ciGiga then
  begin
    dSize := aSize / ciMega;
    Result := FormatFloat(csFormatFloat+csMega, dSize);
    exit;
  end;
  if aSize < ciTera then
  begin
    dSize := aSize / ciGiga;
    Result := FormatFloat(csFormatFloat+csGiga, dSize);
    exit;
  end;
  if aSize < ciPeta then
  begin
    dSize := aSize / ciTera;
    Result := FormatFloat(csFormatFloat+csTera, dSize);
    exit;
  end;
  dSize := aSize / ciPeta;
  Result := FormatFloat(csFormatFloat+csPeta, dSize);
end;

function FormatBytesPerSecond(aSize: Int64): String;
begin
  Result := FormatBytes(aSize) + '/s';
end;

function FormatTorrentState(aState: TqBTorrentState): String;
begin
  Result := csStateUnknown;
  case aState of
    qtsError:       Result := csStateError;
    qtsPausedUp:    Result := csStatePausedUp;
    qtsPausedDl:    Result := csStatePausedDl;
    qtsQueuedUp:    Result := csStateQueuedUp;
    qtsQueuedDl:    Result := csStateQueuedDl;
    qtsUploading:   Result := csStateUploading;
    qtsStalledUp:   Result := csStateStalledUp;
    qtsStalledDl:   Result := csStateStalledDl;
    qtsCheckingUp:  Result := csStateCheckingUp;
    qtsCheckingDl:  Result := csStateCheckingDl;
    qtsDownloading: Result := csStateDownloading;
    qtsMetaDl:      Result := csStateMetaDl;
    else
        Result := csStateUnknown;
  end;
end;

function FormatTrackerStatus(aStatus: TqBTrackerStatus): String;
begin
  Result := qBTrackerStatusToStr(aStatus);
end;

function FormatFilesPriority(aPriority: Integer): String;
begin
  Result := PriorityToStr(aPriority);
end;

end.
