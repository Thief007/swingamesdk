languages = [   
            {"lang": "Pascal", "template": "../../Dist/Pascal/FPC", "main file": "GameMain.pas", "extension": ".pas"}, 
            {"lang": "C", "template": "../../Dist/C/gpp", "main file": "main.c", "extension": ".c"} 
        ]

def get_dist_directory():
	return "../../Dist/"

def get_test_directory():
	return "../../CoreSDK/test/"

def get_test_resource_directory():
	return get_test_directory() + 'Resources/'

def get_how_to_directory():
	return get_dist_directory() + "HowTo/"

def get_how_to_source_directory():
	return get_how_to_directory() + 'Source_Code/'

def get_parser_directory():
	return "../SGWrapperGen/sg/pas_model/"