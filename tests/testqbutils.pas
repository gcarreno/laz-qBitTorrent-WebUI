unit TestqButils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  qBUtils, qBTorrents, qBTorrentsTrackers, qBTorrentsFiles;

type

  { TTestqBUtils }

  TTestqBUtils = class(TTestCase)
  published
    procedure TestFormatBytes;
    procedure TestFormatBytesPerSeconf;
    procedure TestFormatTorrentState;
    procedure TestFormatTrackerStatus;
    procedure TestFormatFilesPriority;
  end;

implementation

const
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

  csStatusWorking         = 'Working';
  csStatusUpdating        = 'Updating...';
  csStatusNotWorking      = 'Not working';
  csStatusNotContactedYet = 'Not contacted yet';
  csStatusUnknown         = 'Unknown';

  csPriorityZero  = 'Do not download';
  csPriorityOne   = 'Normal priority';
  csPriorityTwo   = 'High priority';
  csPrioritySeven = 'Maximal priority';

procedure TTestqBUtils.TestFormatBytes;
begin
  AssertEquals('Format 1,023', '1,023B', FormatBytes(1023));
  AssertEquals('Format 1024',  '1KB',    FormatBytes(1024));
end;

procedure TTestqBUtils.TestFormatBytesPerSeconf;
begin
  AssertEquals('Format 1,023/s', '1,023B/s', FormatBytesPerSecond(1023));
  AssertEquals('Format 1,024/s', '1KB/s',    FormatBytesPerSecond(1024));
end;

procedure TTestqBUtils.TestFormatTorrentState;
begin
  AssertEquals('Torrent State '+csStateUnknown, csStateUnknown, FormatTorrentState(qtsUnknown));
end;

procedure TTestqBUtils.TestFormatTrackerStatus;
begin
  AssertEquals('Tracker Status '+csStatusUnknown, csStatusUnknown, FormatTrackerStatus(qtrsUnknown));
end;

procedure TTestqBUtils.TestFormatFilesPriority;
begin
  AssertEquals('File Priority '+csPriorityZero, csPriorityZero, FormatFilesPriority(0));
end;



initialization

  RegisterTest(TTestqBUtils);
end.

