#!/usr/bin/env python

gl11_exts = set([
    "EXT_blend_logic_op",
    "EXT_copy_texture",
    "EXT_polygon_offset",
    "EXT_subtexture",
    "EXT_texture",
    "EXT_texture_object",
    "EXT_vertex_array",
])

gl12_exts = set([
    "EXT_bgra",
    "EXT_blend_color",
    "EXT_blend_minmax",
    "EXT_convolution",
    "EXT_draw_range_elements",
    "EXT_histogram",
    "EXT_packed_pixels",
    "EXT_rescale_normal",
    "EXT_separate_specular_color",
    "EXT_texture3D",
    "SGI_color_matrix",
    "SGI_color_table",
    "SGIS_texture_edge_clamp",
    "SGIS_texture_lod",
])

gl121_exts = set([
    "SGIS_multitexture",
])

gl13_exts = set([
    "ARB_multisample",
    "ARB_multitexture",
    "ARB_texture_border_clamp",
    "ARB_texture_compression",
    "ARB_texture_cube_map",
    "ARB_texture_env_combine",
    "ARB_texture_env_dot3",
    "ARB_transpose_matrix",
    "EXT_texture_env_add",
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
    "ARB_shader_objects",
    "ARB_shading_language_100",
    "ARB_vertex_shader",
    "EXT_stencil_two_side",
    "EXT_texture_non_power_of_two",
])

gl21_exts = set([
    "ARB_shading_language_120",
    "EXT_pixel_buffer_object",
    "EXT_texture_sRGB",
])

gl30_exts = set([
    "ARB_vertex_array_object",
    "EXT_draw_instanced",
    "EXT_framebuffer_multisample",
    "EXT_framebuffer_object",
    "EXT_framebuffer_sRGB",
])

gl31_exts = set([
    "ARB_copy_buffer",
    "ARB_draw_instanced",
    "ARB_texture_buffer_object",
    "ARB_texture_rectangle",
    "ARB_uniform_buffer_object",
    "NV_primitive_restart",
])

gl32_exts = set([
    "ARB_depth_clamp",
    "ARB_draw_elements_base_vertex",
    "ARB_fragment_coord_conventions",
    "ARB_geometry_shader4",
    "ARB_provoking_vertex",
    "ARB_seamless_cube_map",
    "ARB_sync",
    "ARB_texture_multisample",
    "ARB_vertex_array_bgra",
])

gl33_exts = set([
    "ARB_blend_func_extended",
    "ARB_explicit_attrib_location",
    "ARB_instanced_arrays",
    "ARB_occlusion_query2",
    "ARB_sampler_objects",
    "ARB_texture_rgb10_a2ui",
    "ARB_texture_swizzle",
    "ARB_timer_query",
    "ARB_vertex_type_2_10_10_10_10_rev",
])

gl40_exts = set([
    "ARB_texture_query_lod",
    "ARB_draw_buffers_blend",
    "ARB_draw_indirect",
    "ARB_gpu_shader5",
    "ARB_gpu_shader_fp64",
    "ARB_sample_shading",
    "ARB_shader_subroutine",
    "ARB_tessellation_shader",
    "ARB_texture_buffer_object_rgb32",
    "ARB_texture_cube_map_array",
    "ARB_texture_gather",
    "ARB_transform_feedback2",
    "ARB_transform_feedback3",
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
        for ext in sorted(missing):
            print " --- GL_%s" % ext

def analyze_glxinfo(glxinfo):
    """
    Take glxinfo output and analyze it, reporting useful things.
    """

    ext_begin = glxinfo.index("OpenGL extensions")
    ext_end = glxinfo.index("GLX Visuals")

    exts = parse_exts(glxinfo[ext_begin:ext_end])

    print "Number of extensions: %d" % len(exts)
    check_exts(exts, gl11_exts, "GL 1.1")
    check_exts(exts, gl12_exts, "GL 1.2")
    check_exts(exts, gl121_exts, "GL 1.2.1")
    check_exts(exts, gl13_exts, "GL 1.3")
    check_exts(exts, gl14_exts, "GL 1.4")
    check_exts(exts, gl15_exts, "GL 1.5")
    check_exts(exts, gl20_exts, "GL 2.0")
    check_exts(exts, gl21_exts, "GL 2.1")
    check_exts(exts, gl30_exts, "GL 3.0")
    check_exts(exts, gl31_exts, "GL 3.1")
    check_exts(exts, gl32_exts, "GL 3.2")
    check_exts(exts, gl33_exts, "GL 3.3")
    check_exts(exts, gl40_exts, "GL 4.0")

if __name__ == "__main__":
    import sys
    analyze_glxinfo(sys.stdin.read())
