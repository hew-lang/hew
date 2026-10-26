OUT ?= ../out
DOTNET_CLI_HOME ?= $(OUT)/dotnet-home
NUGET_PACKAGES ?= $(OUT)/nuget

.PHONY: build run

build:
	mkdir -p "$(OUT)/dotnet"
	DOTNET_CLI_HOME="$(DOTNET_CLI_HOME)" NUGET_PACKAGES="$(NUGET_PACKAGES)" dotnet build CalibrationService.csproj --configuration Release --output "$(OUT)/dotnet" --nologo

run: build
	dotnet "$(OUT)/dotnet/calibration-service-dotnet.dll"
