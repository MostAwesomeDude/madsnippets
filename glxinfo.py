gl13_exts = set([
    "ARB_multisample",
    "ARB_multitexture",
    "ARB_texture_border_clamp",
    "ARB_texture_compression",
    "ARB_texture_cube_map",
    "ARB_texture_env_combine",
    "ARB_texture_env_dot3",
    "EXT_texture_env_add",
    "ARB_transpose_matrix",
])

gl14_exts = set([
    "ARB_depth_texture",
    "ARB_shadow",
    "ARB_texture_env_crossbar",
    "ARB_texture_mirrored_repeat",
    "ARB_window_pos",
    "EXT_blend_color",
    "EXT_blend_func_separate",
    "EXT_blend_minmax",
    "EXT_blend_subtract",
    "EXT_fog_coord",
    "EXT_multi_draw_arrays",
    "EXT_point_parameters",
    "EXT_secondary_color",
    "EXT_stencil_wrap",
    "EXT_texture_lod_bias",
    "SGIS_generate_mipmap",
])

gl15_exts = set([
    "ARB_occlusion_query",
    "ARB_vertex_buffer_object",
    "EXT_shadow_funcs",
])

gl20_exts = set([
    "ARB_draw_buffers",
    "ARB_fragment_shader",
    "ARB_point_sprite",
    "EXT_texture_non_power_of_two",
    "ARB_shader_objects",
    "ARB_shading_language_100",
    "EXT_stencil_two_side",
    "ARB_vertex_shader",
])

gl21_exts = set([
    "EXT_pixel_buffer_object",
    "EXT_texture_sRGB",
    "ARB_shading_language_120",
])

def parse_exts(ext_string):
    """
    Split a string of multiple extensions into a set of extensions,
    without the GL_ prefix.
    """

    s = set()
    l = [i.strip(", ") for i in ext_string.split()]
    for i in l:
        if i.startswith("GL_"):
            s.add(i[3:])
    return s

def check_exts(exts, set_to_check, name):
    """
    Checks a set of extensions to see if another set is present,
    and prints helpful analysis.
    """

    missing = set_to_check - exts
    print "%s status: %d/%d present" % (name,
        len(set_to_check) - len(missing), len(set_to_check))
    if missing:
        print " - Missing these extensions:"
        for ext in missing:
            print " --- GL_%s" % ext

def analyze_glxinfo(glxinfo):
    """
    Take glxinfo output and analyze it, reporting useful things.
    """

    ext_begin = glxinfo.index("OpenGL extensions")
    ext_end = glxinfo.index("GLX Visuals")

    exts = parse_exts(glxinfo[ext_begin:ext_end])

    print "Number of extensions: %d" % len(exts)
    check_exts(exts, gl13_exts, "GL 1.3")
    check_exts(exts, gl14_exts, "GL 1.4")
    check_exts(exts, gl15_exts, "GL 1.5")
    check_exts(exts, gl20_exts, "GL 2.0")
    check_exts(exts, gl21_exts, "GL 2.1")

if __name__ == "__main__":
    import sys
    analyze_glxinfo(sys.stdin.read())
