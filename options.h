#ifndef MPLAYER_OPTIONS_H
#define MPLAYER_OPTIONS_H

typedef struct MPOpts {
    char **video_driver_list;
    char **audio_driver_list;
    int fixed_vo;
    int vo_ontop;
    int screen_size_x;
    int screen_size_y;
    int vo_screenwidth;
    int vo_screenheight;
    int force_window_position;
    float force_monitor_aspect;
    float monitor_pixel_aspect;
    int vidmode;
    int fullscreen;
    int vo_dbpp;
    float vo_panscanrange;

    // ranges -100 - 100, 1000 if the vo default should be used
    int vo_gamma_gamma;
    int vo_gamma_brightness;
    int vo_gamma_contrast;
    int vo_gamma_saturation;
    int vo_gamma_hue;

    int osd_level;
    int osd_duration;
    int loop_times;
    int ordered_chapters;
    int edition_id;
    int correct_pts;
    int user_correct_pts;
    int user_pts_assoc_mode;
    int key_fifo_size;
    int doubleclick_time;
    int audio_id;
    int video_id;
    int sub_id;
    float playback_speed;
    struct m_obj_settings *vf_settings;
    int softzoom;
    float movie_aspect;
    float screen_size_xy;
    int flip;
    int vd_use_slices;
    int ass_enabled;
    struct lavc_param {
        int workaround_bugs;
        int error_resilience;
        int error_concealment;
        int gray;
        int vstats;
        int idct_algo;
        int debug;
        int vismv;
        int skip_top;
        int skip_bottom;
        int fast;
        int h264fast;
        char *lowres_str;
        char *skip_loop_filter_str;
        char *skip_idct_str;
        char *skip_frame_str;
        int threads;
        int bitexact;
        char *avopt;
    } lavc_param;
    struct input_conf {
        char *config_file;
        unsigned int ar_delay;
        unsigned int ar_rate;
        char *js_dev;
        char *ar_dev;
        char *in_file;
        int use_joystick;
        int use_lirc;
        int use_lircc;
        int use_ar; // apple remote
        int default_bindings;
    } input;
} MPOpts;

#endif
