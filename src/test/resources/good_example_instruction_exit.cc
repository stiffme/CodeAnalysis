void Example::exmpleFunction3(int a, int b)  {
    HSS_ESM_TRACE_ENTER(HSS_ESM_ENTER, hss_esm_services_appfactory)

    int i = 0;
    if(i == 0)  {
        return;
    } else{
        HSS_ESM_TRACE_EXIT(HSS_ESM_EXIT, hss_esm_services_appfactory);
        return ;
    }
    HSS_ESM_TRACE_EXIT(HSS_ESM_EXIT, hss_esm_services_appfactory);
}