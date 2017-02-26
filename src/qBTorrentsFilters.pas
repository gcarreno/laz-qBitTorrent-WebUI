{
  Torrent Filters Container

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
unit qBTorrentsFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

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

implementation

const
  cNameFilter = 'filter';
  cNameCategory = 'category';
  cNameSort = 'sort';
  cNameReverse = 'reverse';
  cNameLimit = 'limit';
  cNameOffset = 'offset';

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

end.
