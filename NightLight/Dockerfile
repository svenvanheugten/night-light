FROM mcr.microsoft.com/dotnet/sdk:9.0 AS build
WORKDIR /app
COPY NightLight.sln ./
COPY NightLight.Core/NightLight.Core.fsproj NightLight.Core/
COPY NightLight/NightLight.fsproj NightLight/
RUN dotnet restore NightLight/NightLight.fsproj
COPY . ./
RUN dotnet publish NightLight/NightLight.fsproj --no-restore -c Release -o out

FROM mcr.microsoft.com/dotnet/runtime:9.0
WORKDIR /app
COPY --from=build /app/out .
ENTRYPOINT ["dotnet", "NightLight.dll"]
