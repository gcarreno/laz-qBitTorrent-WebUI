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
  csPriorityUnknonwn = 'Unknown';

procedure TTestqBUtils.TestFormatBytes;
begin
  AssertEquals('Format 1K-1', '1,023B', FormatBytes(1024-1));
  AssertEquals('Format 1K',   '1KB',    FormatBytes(1024));
  AssertEquals('Format 1M-1', '1,024KB', FormatBytes(1024*1024-1));
  AssertEquals('Format 1M',   '1MB',    FormatBytes(1024*1024));
  AssertEquals('Format 1G-1', '1,024MB', FormatBytes(1024*1024*1024-1));
  AssertEquals('Format 1G',   '1GB',    FormatBytes(1024*1024*1024));
  AssertEquals('Format 1T-1', '1,024GB', FormatBytes(1024*1024*1024*1024-1));
  AssertEquals('Format 1T',   '1TB',    FormatBytes(1024*1024*1024*1024));
  AssertEquals('Format 1P-1', '1,024TB', FormatBytes(1024*1024*1024*1024*1024-1));
  AssertEquals('Format 1P',   '1PB',    FormatBytes(1024*1024*1024*1024*1024));
end;

procedure TTestqBUtils.TestFormatBytesPerSeconf;
begin
  AssertEquals('Format 1K-1/s', '1,023B/s',  FormatBytesPerSecond(1024-1));
  AssertEquals('Format 1K/s',   '1KB/s',     FormatBytesPerSecond(1024));
  AssertEquals('Format 1M-1/s', '1,024KB/s', FormatBytesPerSecond(1024*1024-1));
  AssertEquals('Format 1M/s',   '1MB/s',     FormatBytesPerSecond(1024*1024));
  AssertEquals('Format 1G-1/s', '1,024MB/s', FormatBytesPerSecond(1024*1024*1024-1));
  AssertEquals('Format 1G/s',   '1GB/s',     FormatBytesPerSecond(1024*1024*1024));
  AssertEquals('Format 1T-1/s', '1,024GB/s', FormatBytesPerSecond(1024*1024*1024*1024-1));
  AssertEquals('Format 1T/s',   '1TB/s',     FormatBytesPerSecond(1024*1024*1024*1024));
  AssertEquals('Format 1P-1/s', '1,024TB/s', FormatBytesPerSecond(1024*1024*1024*1024*1024-1));
  AssertEquals('Format 1P/s',   '1PB/s',     FormatBytesPerSecond(1024*1024*1024*1024*1024));
end;

procedure TTestqBUtils.TestFormatTorrentState;
begin
  AssertEquals('Torrent State '+csStateError,       csStateError,       FormatTorrentState(qtsError));
  AssertEquals('Torrent State '+csStatePausedUp,    csStatePausedUp,    FormatTorrentState(qtsPausedUp));
  AssertEquals('Torrent State '+csStatePausedDl,    csStatePausedDl,    FormatTorrentState(qtsPausedDl));
  AssertEquals('Torrent State '+csStateQueuedUp,    csStateQueuedUp,    FormatTorrentState(qtsQueuedUp));
  AssertEquals('Torrent State '+csStateQueuedDl,    csStateQueuedDl,    FormatTorrentState(qtsQueuedDl));
  AssertEquals('Torrent State '+csStateUploading,   csStateUploading,   FormatTorrentState(qtsUploading));
  AssertEquals('Torrent State '+csStateStalledUp,   csStateStalledUp,   FormatTorrentState(qtsStalledUp));
  AssertEquals('Torrent State '+csStateStalledDl,   csStateStalledDl,   FormatTorrentState(qtsStalledDl));
  AssertEquals('Torrent State '+csStateCheckingUp,  csStateCheckingUp,  FormatTorrentState(qtsCheckingUp));
  AssertEquals('Torrent State '+csStateCheckingDl,  csStateCheckingDl,  FormatTorrentState(qtsCheckingDl));
  AssertEquals('Torrent State '+csStateDownloading, csStateDownloading, FormatTorrentState(qtsDownloading));
  AssertEquals('Torrent State '+csStateMetaDl,      csStateMetaDl,      FormatTorrentState(qtsMetaDl));
  AssertEquals('Torrent State '+csStateUnknown,     csStateUnknown,     FormatTorrentState(qtsUnknown));
end;

procedure TTestqBUtils.TestFormatTrackerStatus;
begin
  AssertEquals('Tracker Status '+csStatusWorking,         csStatusWorking,         FormatTrackerStatus(qtrsWorking));
  AssertEquals('Tracker Status '+csStatusUpdating,        csStatusUpdating,        FormatTrackerStatus(qtrsUpdating));
  AssertEquals('Tracker Status '+csStatusNotWorking,      csStatusNotWorking,      FormatTrackerStatus(qtrsNotWorking));
  AssertEquals('Tracker Status '+csStatusNotContactedYet, csStatusNotContactedYet, FormatTrackerStatus(qtrsNotContactedYet));
  AssertEquals('Tracker Status '+csStatusUnknown,         csStatusUnknown,         FormatTrackerStatus(qtrsUnknown));
end;

procedure TTestqBUtils.TestFormatFilesPriority;
begin
  AssertEquals('File Priority '+csPriorityZero,     csPriorityZero,     FormatFilesPriority(0));
  AssertEquals('File Priority '+csPriorityOne,      csPriorityOne,      FormatFilesPriority(1));
  AssertEquals('File Priority '+csPriorityTwo,      csPriorityTwo,      FormatFilesPriority(2));
  AssertEquals('File Priority '+csPriorityUnknonwn, csPriorityUnknonwn, FormatFilesPriority(3));
  AssertEquals('File Priority '+csPriorityUnknonwn, csPriorityUnknonwn, FormatFilesPriority(4));
  AssertEquals('File Priority '+csPriorityUnknonwn, csPriorityUnknonwn, FormatFilesPriority(4));
  AssertEquals('File Priority '+csPriorityUnknonwn, csPriorityUnknonwn, FormatFilesPriority(6));
  AssertEquals('File Priority '+csPrioritySeven,    csPrioritySeven,    FormatFilesPriority(7));
end;

initialization
  RegisterTest(TTestqBUtils);
end.

