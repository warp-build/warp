#!/bin/bash

rm *.tar.gz
tar czf sample_artifact.tar.gz Manifest.json
tar czf sample_dependency.tar.gz Manifest.json
