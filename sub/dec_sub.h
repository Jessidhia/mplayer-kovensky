struct sh_sub;
struct osd_state;

void sub_decode(struct sh_sub *sh, struct osd_state *osd, void *data,
                int data_len, double pts, double duration);
void sub_init(struct sh_sub *sh, struct osd_state *osd);
