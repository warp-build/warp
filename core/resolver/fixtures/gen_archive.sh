#!/bin/bash

rm *.tar.gz
tar czf sample_artifact.tar.gz Manifest.json tricorder.exe
tar czf sample_dependency.tar.gz Manifest.json dependency
