parameter_xml_file <- system.file("models", "sir.xml", package = "gamar")
outfiles <- call_gama(parameter_xml_file, hpc = 1, output_dir = tempdir())
