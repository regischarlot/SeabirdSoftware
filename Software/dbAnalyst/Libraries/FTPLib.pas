unit FTPLib;

interface

uses
  daGlobals;

function  GetHTTPFile(const fileURL, FileName: String): boolean;
function  GetFTPFile(sparHost, sparUserName, sparPassword, sparRemoteFolder, sparRemoteFileName, sparLocalFileName: String;  bparSecure: boolean; hdlError: TProcError): boolean;
function  TestFTPLink(sparHost, sparUserName, sparPassword: String; bparSecure: boolean): boolean;

implementation

uses
  Windows,
  WinInet,
  sysUtils;

// Tool
//   GetHTTPFile method
//
function GetHTTPFile(const fileURL, FileName: String): boolean;
const
  BufferSize = 1024;
var
  hSession, hURL: HInternet;
  Buffer: array[1..BufferSize] of Byte;
  BufferLen: DWORD;
  f: File;
begin
  Result := False;
  hSession := InternetOpen('dbAnalyst', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if hSession <> nil then
  try
    hURL := InternetOpenURL(hSession, PChar(fileURL), nil, 0, 0, 0);
    if hURL <> nil then
    try
      AssignFile(f, FileName);
      Rewrite(f, 1);
      repeat
        InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen);
        BlockWrite(f, Buffer, BufferLen)
      until BufferLen = 0;
      CloseFile(f);
      Result := True;
    finally
      InternetCloseHandle(hURL)
    end
  finally
    InternetCloseHandle(hSession)
  end
end;

// Tool
//   GetFTPFile method
//
function GetFTPFile(sparHost, sparUserName, sparPassword, sparRemoteFolder, sparRemoteFileName, sparLocalFileName: String;  bparSecure: boolean; hdlError: TProcError): boolean;

  function GetFileSize(value: String): longint;
  var
    f: File of byte;
  begin
    result := 0;
    if FileExists(value) then
    try
      AssignFile(f, value);
      Reset(f);
      result := FileSize(f);
    finally
      CloseFile(f);
    end;
  end;

  function GetError: String;
  var
    p: PChar;
    L, k: DWORD;
  const
    kiSIZE = 2000;
  begin
    result := ksEMPTY;
    p := nil;
    try
      p := strAlloc(kiSIZE + 1);
      if InternetGetLastResponseInfo(L, p, k) then
        result := 'FTP ' + p;
    finally
      strDispose(p);
    end;
  end;

var
  hSession, hURL, hFile: HInternet;
  L1, L2, L3: DWORD;
begin
  result := FALSE;
  hSession := nil;
  try
    hSession := InternetOpen('dbAnalyst', INTERNET_OPEN_TYPE_DIRECT, nil, nil, INTERNET_FLAG_NO_CACHE_WRITE);
    hURL := nil;
    try
      hURL := InternetConnect(hSession, PChar(sparHost), INTERNET_DEFAULT_FTP_PORT, PChar(sparUserName), PChar(sparPassword), INTERNET_SERVICE_FTP, INTERNET_FLAG_EXISTING_CONNECT Or INTERNET_FLAG_PASSIVE, 0);
      // sleep(100);
      if getlasterror = 0 then
      begin
        //
        // Change Directory
        if sparRemoteFolder <> ksEMPTY then
        begin
          FtpSetCurrentDirectory(hURL, PChar(GetNoDrive(sparRemoteFolder)));
          if (getlasterror <> 0) and Assigned(hdlError) then
            hdlError(GetError);
        end;
        //
        // Retrieve File Size
        L3 := 0;
        hFile := nil;
        try
          hFile := FtpOpenFile(hURL, PChar(sparRemoteFileName), GENERIC_READ, FTP_TRANSFER_TYPE_UNKNOWN, 0);
          if hFile <> nil then
            L3 := FtpGetFileSize(hFile, Addr(L1));
          if (getlasterror <> 0) and Assigned(hdlError) then
            hdlError(GetError);
        finally
          InternetCloseHandle(hFile);
        end;
        L2 := GetFileSize(sparLocalFileName);
        //
        // Retrieve File
        if (L3 > 0) and (L3 <> L2) then
          result := FtpGetFile(hURL, PChar(sparRemoteFileName), PChar(sparLocalFileName), False, FILE_ATTRIBUTE_ARCHIVE, FTP_TRANSFER_TYPE_UNKNOWN, 0)
        else if (L3 = L2) then
          result := TRUE;
        if (getlasterror <> 0) and Assigned(hdlError) then
          hdlError(GetError);
      end;
    finally
      InternetCloseHandle(hURL)
    end
  finally
    InternetCloseHandle(hSession)
  end
end;

// Tool
//   TestFTPLink method
//
function TestFTPLink(sparHost, sparUserName, sparPassword: String; bparSecure: boolean): boolean;
var
  hSession, hURL: HInternet;
begin
  result := FALSE;
  hSession := nil;
  try
    try
      hSession := InternetOpen('dbAnalyst', INTERNET_OPEN_TYPE_DIRECT, nil, nil, INTERNET_FLAG_NO_CACHE_WRITE);
      hURL := nil;
      try
        hURL := InternetConnect(hSession, PChar(sparHost), INTERNET_DEFAULT_FTP_PORT, PChar(sparUserName), PChar(sparPassword), INTERNET_SERVICE_FTP, INTERNET_FLAG_EXISTING_CONNECT Or INTERNET_FLAG_PASSIVE, 0);
        result := (hURL <> nil) and (getlasterror = 0);
      finally
        InternetCloseHandle(hURL)
      end
    except
      //
    end;
  finally
    InternetCloseHandle(hSession)
  end
end;

end.
