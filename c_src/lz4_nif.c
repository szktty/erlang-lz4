#include "erl_nif.h"

static ErlNifResourceType* mymodule_RESOURCE = NULL;

typedef struct
{
} mymodule_handle;

// Prototypes
static ERL_NIF_TERM mymodule_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM mymodule_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"new", 0, mymodule_new},
    {"myfunction", 1, mymodule_myfunction}
};

static ERL_NIF_TERM mymodule_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[])
{
    mymodule_handle* handle = enif_alloc_resource(mymodule_RESOURCE,
                                                    sizeof(mymodule_handle));
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}


static ERL_NIF_TERM mymodule_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
}

static void mymodule_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in mymodule_handle */
    /* mymodule_handle* handle = (mymodule_handle*)arg; */
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "mymodule_resource",
                                                     &mymodule_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL)
        return -1;

    mymodule_RESOURCE = rt;

    return 0;
}

ERL_NIF_INIT(mymodule, nif_funcs, &on_load, NULL, NULL, NULL);
