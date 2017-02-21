# laz-qBitTorrent-WebUI
[![Supported FreePascal version: FPC-2.x FPC-3.x](https://img.shields.io/badge/Free%20Pascal-2.x~3.x-blue.svg)](https://github.com/gcarreno/laz-qBitTorrent-WebUI) [![Supported Lazarus version: Laz-0.9.x Laz-1.x](https://img.shields.io/badge/Lazarus-0.9.x~1.x-blue.svg)](https://github.com/gcarreno/laz-qBitTorrent-WebUI) [![GitHub Release](https://img.shields.io/github/release/gcarreno/laz-qBitTorrent-WebUI.svg)](https://github.com/gcarreno/laz-qBitTorrent-WebUI/releases)

A Lazarus component to access the qBitTorrent Web UI

## Dependencies

### Ararat Synapse

This component depends on the [Ararat Synapse](http://synapse.ararat.cz/doku.php/start) Network Lib.

You can get it from one of these options:

 * The main page of the author: [http://synapse.ararat.cz/doku.php/download](http://synapse.ararat.cz/doku.php/download)
 * From Source Forge: [https://sourceforge.net/projects/synalist/](https://sourceforge.net/projects/synalist/)
 * If you are using the [Online Package Manager](http://wiki.freepascal.org/Online_Package_Manager) that comes with Lazarus, it's listed with 2 versions:
   * Version 40.
   * Version 40.1.

I'm using the [Online Package Manager](http://wiki.freepascal.org/Online_Package_Manager) and I'm testing this with version 40.1 of the Synapse Network lib.

I've chosen not to put a package dependency on that lib so you can manage what version you want to use.

The package has a path for my personal version of Synapse, so you may want to fix that once you decide to use it.

## Usage

### GET Methods

#### GetTorrents

Retrieves the torrent list.

```Pascal
uses
  qBitTorrentWebUI;

type
  TForm1 = class(TForm)
    {...}
    Fqb: TqBitTorrentWebUI;
    {...}
  public
    {...}
    procedure GetTorrentsExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.GetTorrentsExecute(Sender: TObject);
begin
  try
    Fqb.Active := True;
    if Fqb.GetTorrents then
    begin
      // Do something after a successfull torrents retrival
    end;
  except
    on E:Exception do
    begin
      // If torrents retrieval fails it will raise an exception
    end;
    Fqb.Active := False;
  end;
end;
```

### Commands

#### Shutdown

This send a command to shutdown(close) the qBitTorrent client.

It also sets `Active` to `False`.

```Pascal
uses
  qBitTorrentWebUI;

type
  TForm1 = class(TForm)
    {...}
    Fqb: TqBitTorrentWebUI;
    {...}
  public
    {...}
    procedure ExecShutdownExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.ExecShutdownExecute(Sender: TObject);
begin
  try
    Fqb.Active := True;
    if Fqb.ExecShutdown then
    begin
      // Do something after a successfull shutdown
    end;
  except
    on E:Exception do
    begin
      // If shutdown fails it will raise an exception
    end;
    Fqb.Active := False;
  end;
end;
```
