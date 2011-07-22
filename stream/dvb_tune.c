/* dvbtune - tune.c

   Copyright (C) Dave Chapman 2001,2002

   Modified for use with MPlayer, for details see the changelog at
   http://svn.mplayerhq.hu/mplayer/trunk/
   $Id$

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
   Or, point your browser to http://www.gnu.org/copyleft/gpl.html

*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/ioctl.h>
#include <poll.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>
#include <errno.h>
#include <linux/dvb/dmx.h>
#include <linux/dvb/frontend.h>
#include "config.h"
#include "dvbin.h"
#include "dvb_tune.h"
#include "mp_msg.h"



int dvb_get_tuner_type(int fe_fd)
{
  struct dvb_frontend_info fe_info;

  int res;

  res = ioctl(fe_fd, FE_GET_INFO, &fe_info);
  if(res < 0)
  {
  	mp_msg(MSGT_DEMUX, MSGL_ERR, "FE_GET_INFO error: %d, FD: %d\n\n", errno, fe_fd);
	return 0;
  }

  switch(fe_info.type)
  {
	case FE_OFDM:
      mp_msg(MSGT_DEMUX, MSGL_V, "TUNER TYPE SEEMS TO BE DVB-T\n");
	  return TUNER_TER;

	case FE_QPSK:
      mp_msg(MSGT_DEMUX, MSGL_V, "TUNER TYPE SEEMS TO BE DVB-S\n");
	  return TUNER_SAT;

	case FE_QAM:
      mp_msg(MSGT_DEMUX, MSGL_V, "TUNER TYPE SEEMS TO BE DVB-C\n");
	  return TUNER_CBL;

#ifdef DVB_ATSC
	case FE_ATSC:
      mp_msg(MSGT_DEMUX, MSGL_V, "TUNER TYPE SEEMS TO BE DVB-ATSC\n");
	  return TUNER_ATSC;
#endif
	default:
	  mp_msg(MSGT_DEMUX, MSGL_ERR, "UNKNOWN TUNER TYPE\n");
	  return 0;
  }

}

int dvb_open_devices(dvb_priv_t *priv, int n, int demux_cnt)
{
	int i;
	char frontend_dev[32], dvr_dev[32], demux_dev[32];

	sprintf(frontend_dev, "/dev/dvb/adapter%d/frontend0", n);
	sprintf(dvr_dev, "/dev/dvb/adapter%d/dvr0", n);
	sprintf(demux_dev, "/dev/dvb/adapter%d/demux0", n);
	priv->fe_fd = open(frontend_dev, O_RDWR | O_NONBLOCK);
	if(priv->fe_fd < 0)
	{
		mp_msg(MSGT_DEMUX, MSGL_ERR, "ERROR OPENING FRONTEND DEVICE %s: ERRNO %d\n", frontend_dev, errno);
		return 0;
	}
	priv->demux_fds_cnt = 0;
	mp_msg(MSGT_DEMUX, MSGL_V, "DVB_OPEN_DEVICES(%d)\n", demux_cnt);
	for(i = 0; i < demux_cnt; i++)
	{
		priv->demux_fds[i] = open(demux_dev, O_RDWR | O_NONBLOCK);
		if(priv->demux_fds[i] < 0)
		{
			mp_msg(MSGT_DEMUX, MSGL_ERR, "ERROR OPENING DEMUX 0: %d\n", errno);
			return 0;
		}
		else
		{
			mp_msg(MSGT_DEMUX, MSGL_V, "OPEN(%d), file %s: FD=%d, CNT=%d\n", i, demux_dev, priv->demux_fds[i], priv->demux_fds_cnt);
			priv->demux_fds_cnt++;
		}
	}


	priv->dvr_fd = open(dvr_dev, O_RDONLY| O_NONBLOCK);
	if(priv->dvr_fd < 0)
	{
		mp_msg(MSGT_DEMUX, MSGL_ERR, "ERROR OPENING DVR DEVICE %s: %d\n", dvr_dev, errno);
		return 0;
	}

	return 1;
}


int dvb_fix_demuxes(dvb_priv_t *priv, int cnt)
{
	int i;
	char demux_dev[32];

	sprintf(demux_dev, "/dev/dvb/adapter%d/demux0", priv->card);
	mp_msg(MSGT_DEMUX, MSGL_V, "FIX %d -> %d\n", priv->demux_fds_cnt, cnt);
	if(priv->demux_fds_cnt >= cnt)
	{
		for(i = priv->demux_fds_cnt-1; i >= cnt; i--)
		{
			mp_msg(MSGT_DEMUX, MSGL_V, "FIX, CLOSE fd(%d): %d\n", i, priv->demux_fds[i]);
			close(priv->demux_fds[i]);
		}
		priv->demux_fds_cnt = cnt;
	}
	else if(priv->demux_fds_cnt < cnt)
	{
		for(i = priv->demux_fds_cnt; i < cnt; i++)
		{
			priv->demux_fds[i] = open(demux_dev, O_RDWR | O_NONBLOCK);
			mp_msg(MSGT_DEMUX, MSGL_V, "FIX, OPEN fd(%d): %d\n", i, priv->demux_fds[i]);
			if(priv->demux_fds[i] < 0)
			{
				mp_msg(MSGT_DEMUX, MSGL_ERR, "ERROR OPENING DEMUX 0: %d\n", errno);
				return 0;
			}
			else
				priv->demux_fds_cnt++;
		}
	}

	return 1;
}

int dvb_set_ts_filt(int fd, uint16_t pid, dmx_pes_type_t pestype)
{
	int i;
	struct dmx_pes_filter_params pesFilterParams;

	pesFilterParams.pid     = pid;
	pesFilterParams.input   = DMX_IN_FRONTEND;
	pesFilterParams.output  = DMX_OUT_TS_TAP;
	pesFilterParams.pes_type = pestype;
	pesFilterParams.flags   = DMX_IMMEDIATE_START;

	errno = 0;
	if ((i = ioctl(fd, DMX_SET_PES_FILTER, &pesFilterParams)) < 0)
	{
		mp_msg(MSGT_DEMUX, MSGL_ERR, "ERROR IN SETTING DMX_FILTER %i for fd %d: ERRNO: %d", pid, fd, errno);
		return 0;
	}

	mp_msg(MSGT_DEMUX, MSGL_V, "SET PES FILTER ON PID %d to fd %d, RESULT: %d, ERRNO: %d\n", pid, fd, i, errno);
	return 1;
}


int dvb_demux_stop(int fd)
{
	int i;
	i = ioctl(fd, DMX_STOP);

	mp_msg(MSGT_DEMUX, MSGL_DBG2, "STOPPING FD: %d, RESULT: %d\n", fd, i);

	return i == 0;
}


int dvb_demux_start(int fd)
{
	int i;
	i = ioctl(fd, DMX_START);

	mp_msg(MSGT_DEMUX, MSGL_DBG2, "STARTING FD: %d, RESULT: %d\n", fd, i);

	return i == 0;
}


static void print_status(fe_status_t festatus)
{
	mp_msg(MSGT_DEMUX, MSGL_V, "FE_STATUS:");
	if (festatus & FE_HAS_SIGNAL) mp_msg(MSGT_DEMUX, MSGL_V," FE_HAS_SIGNAL");
	if (festatus & FE_TIMEDOUT) mp_msg(MSGT_DEMUX, MSGL_V, " FE_TIMEDOUT");
	if (festatus & FE_HAS_LOCK) mp_msg(MSGT_DEMUX, MSGL_V, " FE_HAS_LOCK");
	if (festatus & FE_HAS_CARRIER) mp_msg(MSGT_DEMUX, MSGL_V, " FE_HAS_CARRIER");
	if (festatus & FE_HAS_VITERBI) mp_msg(MSGT_DEMUX, MSGL_V, " FE_HAS_VITERBI");
	if (festatus & FE_HAS_SYNC) mp_msg(MSGT_DEMUX, MSGL_V, " FE_HAS_SYNC");
	mp_msg(MSGT_DEMUX, MSGL_V, "\n");
}


static int check_status(int fd_frontend, int tmout)
{
	int32_t strength;
	fe_status_t festatus;
	struct pollfd pfd[1];
	int ok=0, locks=0;
	time_t tm1, tm2;

	pfd[0].fd = fd_frontend;
	pfd[0].events = POLLPRI;

	mp_msg(MSGT_DEMUX, MSGL_V, "Getting frontend status\n");
	tm1 = tm2 = time((time_t*) NULL);
	while(!ok)
	{
		festatus = 0;
		if(poll(pfd,1,tmout*1000) > 0)
		{
			if (pfd[0].revents & POLLPRI)
			{
				if(ioctl(fd_frontend, FE_READ_STATUS, &festatus) >= 0)
					if(festatus & FE_HAS_LOCK)
						locks++;
			}
		}
		usleep(10000);
		tm2 = time((time_t*) NULL);
		if((festatus & FE_TIMEDOUT) || (locks >= 2) || (tm2 - tm1 >= tmout))
			ok = 1;
	}

	if(festatus & FE_HAS_LOCK)
	{
		strength=0;
		if(ioctl(fd_frontend,FE_READ_BER,&strength) >= 0)
		mp_msg(MSGT_DEMUX, MSGL_V, "Bit error rate: %d\n",strength);

		strength=0;
		if(ioctl(fd_frontend,FE_READ_SIGNAL_STRENGTH,&strength) >= 0)
		mp_msg(MSGT_DEMUX, MSGL_V, "Signal strength: %d\n",strength);

		strength=0;
		if(ioctl(fd_frontend,FE_READ_SNR,&strength) >= 0)
		mp_msg(MSGT_DEMUX, MSGL_V, "SNR: %d\n",strength);

		strength=0;
		if(ioctl(fd_frontend,FE_READ_UNCORRECTED_BLOCKS,&strength) >= 0)
		mp_msg(MSGT_DEMUX, MSGL_V, "UNC: %d\n",strength);

		print_status(festatus);
	}
	else
	{
		mp_msg(MSGT_DEMUX, MSGL_ERR, "Not able to lock to the signal on the given frequency, timeout: %d\n", tmout);
		return -1;
	}
	return 0;
}


struct diseqc_cmd {
   struct dvb_diseqc_master_cmd cmd;
   uint32_t wait;
};

static int diseqc_send_msg(int fd, fe_sec_voltage_t v, struct diseqc_cmd *cmd,
		     fe_sec_tone_mode_t t, fe_sec_mini_cmd_t b)
{
   if(ioctl(fd, FE_SET_TONE, SEC_TONE_OFF) == -1)
    return -1;
   if(ioctl(fd, FE_SET_VOLTAGE, v) == -1)
    return -1;
   usleep(15 * 1000);
   if(ioctl(fd, FE_DISEQC_SEND_MASTER_CMD, &cmd->cmd) == -1)
    return -1;
   usleep(cmd->wait * 1000);
   usleep(15 * 1000);
   if(ioctl(fd, FE_DISEQC_SEND_BURST, b) == -1)
    return -1;
   usleep(15 * 1000);
   if(ioctl(fd, FE_SET_TONE, t) == -1)
    return -1;

    return 0;
}

/* digital satellite equipment control,
 * specification is available from http://www.eutelsat.com/
 */
static int do_diseqc(int secfd, int sat_no, int polv, int hi_lo)
{
   struct diseqc_cmd cmd =  { {{0xe0, 0x10, 0x38, 0xf0, 0x00, 0x00}, 4}, 0 };

   /* param: high nibble: reset bits, low nibble set bits,
    * bits are: option, position, polarizaion, band
    */
   cmd.cmd.msg[3] =
       0xf0 | (((sat_no * 4) & 0x0f) | (hi_lo ? 1 : 0) | (polv ? 0 : 2));

   return diseqc_send_msg(secfd, polv ? SEC_VOLTAGE_13 : SEC_VOLTAGE_18,
		   &cmd, hi_lo ? SEC_TONE_ON : SEC_TONE_OFF,
		   (sat_no / 4) % 2 ? SEC_MINI_B : SEC_MINI_A);
}

static int tune_it(int fd_frontend, int fd_sec, unsigned int freq, unsigned int srate, char pol, int tone,
	fe_spectral_inversion_t specInv, unsigned int diseqc, fe_modulation_t modulation, fe_code_rate_t HP_CodeRate,
	fe_transmit_mode_t TransmissionMode, fe_guard_interval_t guardInterval, fe_bandwidth_t bandwidth,
	fe_code_rate_t LP_CodeRate, fe_hierarchy_t hier, int timeout)
{
  int res, hi_lo = 0, dfd;
  struct dvb_frontend_parameters feparams;
  struct dvb_frontend_info fe_info;

  mp_msg(MSGT_DEMUX, MSGL_V,  "TUNE_IT, fd_frontend %d, fd_sec %d\nfreq %lu, srate %lu, pol %c, tone %i, specInv, diseqc %u, fe_modulation_t modulation,fe_code_rate_t HP_CodeRate, fe_transmit_mode_t TransmissionMode,fe_guard_interval_t guardInterval, fe_bandwidth_t bandwidth\n",
    fd_frontend, fd_sec, (long unsigned int)freq, (long unsigned int)srate, pol, tone, diseqc);


  memset(&feparams, 0, sizeof(feparams));
  if ( (res = ioctl(fd_frontend,FE_GET_INFO, &fe_info) < 0))
  {
        mp_msg(MSGT_DEMUX, MSGL_FATAL, "FE_GET_INFO FAILED\n");
        return -1;
  }

  mp_msg(MSGT_DEMUX, MSGL_V, "Using DVB card \"%s\"\n", fe_info.name);

  switch(fe_info.type)
  {
    case FE_OFDM:
      if (freq < 1000000) freq*=1000UL;
      feparams.frequency=freq;
      feparams.inversion=specInv;
      feparams.u.ofdm.bandwidth=bandwidth;
      feparams.u.ofdm.code_rate_HP=HP_CodeRate;
      feparams.u.ofdm.code_rate_LP=LP_CodeRate;
      feparams.u.ofdm.constellation=modulation;
      feparams.u.ofdm.transmission_mode=TransmissionMode;
      feparams.u.ofdm.guard_interval=guardInterval;
      feparams.u.ofdm.hierarchy_information=hier;
      mp_msg(MSGT_DEMUX, MSGL_V, "tuning DVB-T to %d Hz, bandwidth: %d\n",freq, bandwidth);
      break;
    case FE_QPSK:
      if (freq > 2200000)
      {
        // this must be an absolute frequency
        if (freq < SLOF)
        {
          freq = feparams.frequency=(freq-LOF1);
          hi_lo = 0;
        }
        else
        {
          freq = feparams.frequency=(freq-LOF2);
          hi_lo = 1;
        }
      }
      else
      {
        // this is an L-Band frequency
       feparams.frequency=freq;
      }

      feparams.inversion=specInv;
      feparams.u.qpsk.symbol_rate=srate;
      feparams.u.qpsk.fec_inner=HP_CodeRate;
      dfd = fd_frontend;

      mp_msg(MSGT_DEMUX, MSGL_V, "tuning DVB-S to Freq: %u, Pol: %c Srate: %d, 22kHz: %s, LNB:  %d\n",freq,pol,srate,hi_lo ? "on" : "off", diseqc);

      if(do_diseqc(dfd, diseqc, (pol == 'V' ? 1 : 0), hi_lo) == 0)
          mp_msg(MSGT_DEMUX, MSGL_V, "DISEQC SETTING SUCCEDED\n");
      else
      {
          mp_msg(MSGT_DEMUX, MSGL_ERR, "DISEQC SETTING FAILED\n");
          return -1;
      }
      break;
    case FE_QAM:
      mp_msg(MSGT_DEMUX, MSGL_V, "tuning DVB-C to %d, srate=%d\n",freq,srate);
      feparams.frequency=freq;
      feparams.inversion=specInv;
      feparams.u.qam.symbol_rate = srate;
      feparams.u.qam.fec_inner = HP_CodeRate;
      feparams.u.qam.modulation = modulation;
      break;
#ifdef DVB_ATSC
    case FE_ATSC:
      mp_msg(MSGT_DEMUX, MSGL_V, "tuning ATSC to %d, modulation=%d\n",freq,modulation);
      feparams.frequency=freq;
      feparams.u.vsb.modulation = modulation;
      break;
#endif
    default:
      mp_msg(MSGT_DEMUX, MSGL_V, "Unknown FE type. Aborting\n");
      return 0;
  }
  usleep(100000);

  if(ioctl(fd_frontend,FE_SET_FRONTEND,&feparams) < 0)
  {
    mp_msg(MSGT_DEMUX, MSGL_ERR, "ERROR tuning channel\n");
    return -1;
  }

  return check_status(fd_frontend, timeout);
}


int dvb_tune(dvb_priv_t *priv, int freq, char pol, int srate, int diseqc, int tone,
		fe_spectral_inversion_t specInv, fe_modulation_t modulation, fe_guard_interval_t guardInterval,
		fe_transmit_mode_t TransmissionMode, fe_bandwidth_t bandWidth, fe_code_rate_t HP_CodeRate,
		fe_code_rate_t LP_CodeRate, fe_hierarchy_t hier, int timeout)
{
	int ris;

	mp_msg(MSGT_DEMUX, MSGL_INFO, "dvb_tune Freq: %lu\n", (long unsigned int) freq);

		ris = tune_it(priv->fe_fd, priv->sec_fd, freq, srate, pol, tone, specInv, diseqc, modulation, HP_CodeRate, TransmissionMode, guardInterval, bandWidth, LP_CodeRate, hier, timeout);

	if(ris != 0)
		mp_msg(MSGT_DEMUX, MSGL_INFO, "dvb_tune, TUNING FAILED\n");

	return ris == 0;
}
