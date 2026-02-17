# ============================================================
# Build all Docker images locally and push to Docker Hub
#
# USAGE: .\build-and-push.ps1
# ============================================================

$ErrorActionPreference = "Stop"
$ROOT = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)

# If run directly from infra/, adjust ROOT
if (-not (Test-Path "$ROOT\java-maven")) {
    $ROOT = Split-Path -Parent $PSCommandPath | Split-Path -Parent
}

$DOCKER_USER = "ktharankumar"

$services = @(
    @{ Name = "product-service"; Context = "$ROOT\java-maven\products" },
    @{ Name = "cart-service";    Context = "$ROOT\java-maven\carts" },
    @{ Name = "user-service";    Context = "$ROOT\java-maven\users" },
    @{ Name = "shopper-ui";      Context = "$ROOT\shopper-ui" }
)

Write-Host "`n=== Building & pushing images to Docker Hub ===" -ForegroundColor Cyan

foreach ($svc in $services) {
    $image = "$DOCKER_USER/$($svc.Name):latest"
    Write-Host "`n--- Building $image ---" -ForegroundColor Yellow
    docker build -t $image $svc.Context
    if ($LASTEXITCODE -ne 0) { Write-Host "FAILED: $image" -ForegroundColor Red; exit 1 }

    Write-Host "--- Pushing $image ---" -ForegroundColor Yellow
    docker push $image
    if ($LASTEXITCODE -ne 0) { Write-Host "FAILED to push: $image" -ForegroundColor Red; exit 1 }
}

Write-Host "`n=== All images pushed successfully! ===" -ForegroundColor Green
Write-Host "Now SSH to EC2 and run: ./deploy-ec2.sh`n"
