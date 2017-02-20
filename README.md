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

### Authentication

#### Login

This allows you to login to the WebUI.

You have to login before you can do anything else.

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
    procedure LoginExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.LoginExecute(Sender: TObject);
begin
  try
    if Fqb.Login then
    begin
      // Do something after a successfull login
    end;
  except
    on E:Exception do
    begin
      // If login fails it will raise an exception
    end;
  end;
end;
```

#### Logout

This will log you out from the WebbUI.

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
    procedure LogoutExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.LogoutExecute(Sender: TObject);
begin
  try
    if Fqb.Logout then
    begin
      // Do something after a successfull logout
    end;
  except
    on E:Exception do
    begin
      // If logout fails it will raise an exception
    end;
  end;
end;
```

### GET Methods

#### GetApiVersion

This will retrieve the WebUI version.

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
    procedure GetApiVersionExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.GetApiVersionExecute(Sender: TObject);
var
  sVersion: String;
begin
  try
    sversion := Fqb.GetApiVersion
     // Do something with the version
  except
    on E:Exception do
    begin
      // If GetApiVersion fails it will raise an exception
    end;
  end;
end;
```

#### GetMinApiVersion

This will retrieve the minimum WebUI version it supports.

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
    procedure GetMinApiVersionExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.GetMinApiVersionExecute(Sender: TObject);
var
  sVersion: String;
begin
  try
    sversion := Fqb.GetMinApiVersion
     // Do something with the version
  except
    on E:Exception do
    begin
      // If GetMinApiVersion fails it will raise an exception
    end;
  end;
end;
```

#### GetqBitTorrentVersion

This will retrieve qBitTorrent's version.

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
    procedure GetqBitTorrentVersionExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.GetqBitTorrentVersionExecute(Sender: TObject);
var
  sVersion: String;
begin
  try
    sversion := Fqb.GetqBitTorrentVersion
     // Do something with the version
  except
    on E:Exception do
    begin
      // If GetqBitTorrentVersion fails it will raise an exception
    end;
  end;
end;
```

### Commands

#### Shutdown

This send a command to shutdown(close) the qBitTorrent client.

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
var
  sVersion: String;
begin
  try
    sversion := Fqb.GetqBitTorrentVersion
     // Do something after a successfull shutdown
  except
    on E:Exception do
    begin
      // If shutdown fails it will raise an exception
    end;
  end;
end;
```
