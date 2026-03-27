Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# $here es .../MVD/04_tutorial
$here = $PSScriptRoot 

Set-Location $here
# Subimos un nivel para estar en la raíz del proyecto (MVD)
Set-Location .. 

Write-Host "Ubicación forzada para el pipeline: $(Get-Location)" -ForegroundColor Yellow

# Definimos explícitamente la carpeta de scripts para evitar confusiones
$scriptsDir = Join-Path $here "scripts"

Write-Host "Iniciando Pipeline desde: $here" -ForegroundColor Cyan

try {
    # Ejecutamos los scripts usando la ruta absoluta calculada por PowerShell
    & Rscript (Join-Path $scriptsDir "download_data.R")
    & Rscript (Join-Path $scriptsDir "wrangling_data.R")
    & Rscript (Join-Path $scriptsDir "make_figures.R")
    
    Write-Host "Pipeline finalizado con éxito en /04_tutorial" -ForegroundColor Green
} catch {
    Write-Error "Fallo en el Pipeline: $_"
    exit 1
}