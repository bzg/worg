;;; -*- coding: utf-8-unix -*-
;;; org-player.el - Play audio and video files in org-mode hyperlinks
;;;
;;; Author: Paul Sexton <eeeickythump@gmail.com>
;;; Version: 1.0.0
;;; Repository at http://bitbucket.org/eeeickythump/org-player/
;;;
;;;
;;; Synopsis
;;; ========
;;;
;;; I was learning a language, and wanted to include hyperlinks to audio files
;;; within my org document, and to be able to play each file by clicking on its
;;; link. I wrote the code in this file, which makes org mode use an Emacs
;;; media player library, Bongo, to play audio and video files contained in
;;; 'file:' hyperlinks.
;;;
;;; Installation
;;; ============
;;;
;;; First you will need to install Bongo, which you can download from:
;;; https://github.com/dbrock/bongo
;;;
;;; You will also need to ensure you have an actual media player program
;;; installed, and you need to tell Bongo which program to use to play files. I
;;; have tested org-player with MPlayer, an open source media player which
;;; works well on Windows and Linux. VLC does not work with Bongo on Windows at
;;; the time of writing.
;;;
;;; You can obtain MPlayer from:
;;; http://www.mplayerhq.hu/design7/news.html
;;;
;;; Ensure you have Bongo set up and working. Once this is achieved, installing
;;; org-player is easy - just put the file somewhere in your Emacs load-path
;;; and put (require 'org-player) in your .emacs file.
;;;
;;; Usage
;;; =====
;;;
;;; Clicking on a link such as
;;;
;;;   [[file:/path/to/song.mp3]]
;;;
;;; adds it to the active Bongo playlist and immediately starts playing
;;; it. Playback can be paused, fast-forwarded etc using Bongo commands (see
;;; below).
;;;
;;; Links can also specify track positions. When a link contains a track
;;; position, playback will start at that position in the track. For example:
;;;
;;; [[file:/path/to/song.mp3::2:43]]      Starts playback at 2 min 43 sec.
;;; [[file:/path/to/song.mp3::1:10:45]]   Starts playback at 1 hr 10 min 45 sec.
;;; [[file:/path/to/song.mp3::3m15s]]     Starts playback at 3 min 15 sec.
;;; [[file:/path/to/song.mp3::49s]]       Starts playback at 0 min 49 sec.
;;; [[file:/path/to/song.mp3::1h21m10s]]  Starts playback at 1 hr 21 min 10 sec.
;;;
;;; Controlling playback
;;; ====================
;;;
;;; When Bongo plays a file it puts some icons in the modeline that resemble the
;;; well-known symbols for 'play', 'stop', 'rewind' and so on, and which can be
;;; used to control playback using the mouse. I found these worked erratically when
;;; outside the actual Bongo playlist buffer, so I have instead bound some org-mode
;;; keys (ctrl + numpad keys) to the relevant functions.  These are:
;;;
;;; C-<keypad 0>     pause/resume
;;; C-<keypad .>     stop, or restart playback from beginning
;;; C-<keypad />     show track info
;;; C-<keypad 4>     skip back 10 seconds
;;; C-<keypad 6>     skip forward 10 seconds
;;; C-<keypad 9>     increase volume (requires separate 'volume' library
;;;                  at https://github.com/dbrock/volume-el )
;;; C-<keypad 3>     decrease volume
;;;
;;; Note that these keys (and the modeline icons, if they work) act on the
;;; track that is active in the Bongo playlist. This will always be the last
;;; track that you added by clicking on a link in org-mode, unless you alter
;;; the Bongo playlist outside org mode.
;;;
;;; Also note that these keys are only bound within org mode buffers.
;;;
;;; Notes
;;; =====
;;;
;;; I have only tested this with MP3 files, but it ought to work with
;;; video as well, as both MPlayer and Bongo claim to work with video files.
;;;
;;; Future plans
;;; ============
;;;
;;; If anyone wants to integrate this code with EMMS, another popular Emacs
;;; media player library, contributions would be welcome.


(require 'org)
(require 'bongo)

(defvar org-player-file-extensions-regexp
  (concat "\\." (regexp-opt
                 (append bongo-audio-file-name-extensions
                         bongo-video-file-name-extensions))))

(add-to-list 'org-file-apps
             (cons (concat org-player-file-extensions-regexp "$")
                   '(org-player-play-file file)))

(add-to-list 'org-file-apps
             (cons (concat org-player-file-extensions-regexp
                           "::[0-9]+:[0-9]+\\(:[0-9]+\\|\\)")
                   '(org-player-play-file file search)))

(add-to-list 'org-file-apps
             (cons (concat org-player-file-extensions-regexp
                           "::[0-9]+[HhMmSs]\\([0-9]+[MmSs]\\|\\)\\([0-9]+[MmSs]\\|\\)")
                   '(org-player-play-file file search)))


(defun org-player-string-to-time (str)
  "Understands any of the following formats: MM:SS, HH:MM:SS, NNhNNmNNs,
NNhNNm, NNmNNs, NNh, NNm, NNs."
  (let ((str (substring-no-properties str))
        hours mins secs)
    (cond
     ((string-match "\\([0-9]+\\):\\([0-9]+\\)\\(:[0-9.]+\\|\\)" str)
      (if (string= "" (match-string 3 str)) ; XX:YY
          (setq mins (match-string 1 str)
                secs (match-string 2 str))
        (setq hours (match-string 1 str) ; XX:YY:ZZ
              mins (match-string 2 str)
              secs (subseq (match-string 3 str) 1))))
     ((eq (string-match "[0-9]+[HhMmSs]" str)
          (string-match "\\([0-9]+[Hh]\\|\\)\\([0-9]+[Mm]\\|\\)\\([0-9.]+[Ss]\\|\\)"
                        str))
      (setq hours (match-string 1 str)
            mins (match-string 2 str)
            secs (match-string 3 str))
      (unless (string= "" hours)
        (setq hours (subseq hours 0 (1- (length hours)))))
      (unless (string= "" mins)
        (setq mins (subseq mins 0 (1- (length mins)))))
      (unless (string= "" secs)
        (setq secs (subseq secs 0 (1- (length secs))))))
     (t
      (error "Unrecognised track position specifier: %S" str)))
    (setq hours (if (or (null hours) (string= "" hours))
                    0 (read hours)))
    (setq mins (if (or (null mins) (string= "" mins))
                    0 (read mins)))
    (setq secs (if (or (null secs) (string= "" secs))
                    0 (read secs)))
    (list hours mins secs)))


(defun org-player-play-file (filename &optional pos)
  "POS, if given, is a string that specifies a track position where
playback should begin. This string is taken from the portion of the
hyperlink that follows a double colon. For example:

  [[file:song.mp3::03:22]]
  [[file:song.mp3::1h24m3s]]

See `org-player-string-to-time' for a description of the format of
the POS string."
  (let ((seek (if pos
                  (destructuring-bind (hours mins secs)
                      (org-player-string-to-time pos)
                    (+ secs
                       (* mins 60)
                       (* hours 60 60))))))
    (with-bongo-buffer
      (bongo-insert-file filename)
      (backward-char)
      (org-player-start/stop 'play seek)
      (when seek
        (bongo-seek-to seek)))))


(defun org-player-get-track-info ()
  "Returns a list: (TRACKTITLE ALBUMNAME ARTISTNAME ELAPSEDSECS LENGTHSECS)"
  (with-bongo-buffer
    (let* ((info (bongo-line-infoset))
           (track (cdr (assoc 'track info))))
      (message "%s" info)
      (list (cdr (assoc 'title track))
            (cdr (assoc 'album info))
            (cdr (assoc 'artist info))
            (or (bongo-elapsed-time) 0)
            (or (bongo-total-time) 0)))))


(defun org-player-print-track-info ()
  "Print out some info about the active track, in the minibuffer."
  (interactive)
  (destructuring-bind (title album artist elapsed total)
      (org-player-get-track-info)
    (message "%s" (list title album artist elapsed total))
    (cond
     ((null title)
      (message "No active track."))
     (t
      (message "Track: %s
Album: %s / %s
%s / %s (%d%%) %s"
               title album artist
               (format "%02d:%02d" (floor elapsed 60) (mod elapsed 60))
               (format "%02d:%02d" (floor total 60) (mod total 60))
               (/ (* elapsed 100) total)
               (cond
                ((bongo-paused-p) "(paused)")
                ((bongo-playing-p) "(playing)")
                (t "(stopped)"))
               )))))


(defun org-player-pause/resume ()
  (interactive)
  (destructuring-bind (title album artist elapsed total)
      (org-player-get-track-info)
    (when title
      (message "%02d:%02d/%02d:%02d %s %s (%s / %s)"
               (floor elapsed 60) (mod elapsed 60)
               (floor total 60) (mod total 60)
               (if (bongo-paused-p) "Unpaused:" "Paused:")
               title album artist)))
    (bongo-pause/resume))


(defun org-player-start/stop (&optional force starting-pos)
  (interactive)
  (destructuring-bind (title album artist elapsed total)
      (org-player-get-track-info)
    (when title
      (cond
       ((eq force 'play)
        (bongo-play))
       ((eq force 'stop)
        (bongo-stop))
       (t
        (bongo-start/stop)))
      (let ((msg (format "%02d:%02d/%02d:%02d %s %s (%s / %s)"
                         (floor (or starting-pos 0) 60)
                         (mod (or starting-pos 0) 60)
                         (floor total 60) (mod total 60)
                         (if (bongo-playing-p) "Playing:" "Stopped:")
                         title album artist)))
        (message msg)))))


;; Numpad Ctrl-0: pause/resume
(define-key org-mode-map (kbd "<C-kp-0>") 'org-player-pause/resume)
(define-key org-mode-map (kbd "<C-kp-insert>") 'org-player-pause/resume)
;; Numpad Ctrl-.: stop current track, or restart from beginning if stopped
(define-key org-mode-map (kbd "<C-kp-decimal>") 'org-player-start/stop)
(define-key org-mode-map (kbd "<C-kp-delete>") 'org-player-start/stop)
;; Numpad Ctrl-PgUp, Ctrl-PgDn: raise/lower volume
(define-key org-mode-map (kbd "<C-kp-prior>") 'volume-raise)
(define-key org-mode-map (kbd "<C-kp-next>") 'volume-lower)
(define-key org-mode-map (kbd "<C-kp-9>") 'volume-raise)
(define-key org-mode-map (kbd "<C-kp-3>") 'volume-lower)
;; Numpad Ctrl-left, Ctrl-right: skip back/forward 10 seconds
(define-key org-mode-map (kbd "<C-kp-left>") 'bongo-seek-backward-10)
(define-key org-mode-map (kbd "<C-kp-right>") 'bongo-seek-forward-10)
(define-key org-mode-map (kbd "<C-kp-4>") 'bongo-seek-backward-10)
(define-key org-mode-map (kbd "<C-kp-6>") 'bongo-seek-forward-10)
;; Ctrl-/: show track info
(define-key org-mode-map (kbd "<C-kp-divide>") 'org-player-print-track-info)


(provide 'org-player)
