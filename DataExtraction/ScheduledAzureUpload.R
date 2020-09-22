library(AzureStor)

bl_endp_key <- storage_endpoint("https://jiraanalytics.blob.core.windows.net", key="JCuArdxsr0zfObDD7AKXGn3ywKk/J63NXZWYPR+Tky99MAbPJZeUiQ19NYTnteuo5DxSGOikzgx8F/rk8umj6w==")


cont <- storage_container(bl_endp_key, "regularflow")

storage_upload(cont, "RegularFlow.csv")

storage_upload(cont, "RegularFlowAggregated_View.csv")