#!/bin/sh
dotnet publish csharp/basic -c Release -r linux-x64 /p:PublishAot=true
./basic
