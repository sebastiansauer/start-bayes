import subprocess
import os

ffmpeg = "/home/sebastian-sauer/.local/bin/ffmpeg"
output_dir = "/tmp/bayes_video_build"
final_video = "/home/sebastian-sauer/Dokumente/github/04_start-bayes/bayes_erklaervideo_stichproben_mcmc.mp4"
artifact_dir = "/home/sebastian-sauer/.gemini/antigravity/brain/6bf78951-c6a2-4744-b602-0f464d3ca240"

concat_list_path = os.path.join(output_dir, "concat_list.txt")

with open(concat_list_path, "w") as clist:
    for i in range(1, 11):
        slide_png = f"{output_dir}/slide_{i:02d}.png"
        audio_mp3 = f"{output_dir}/audio_{i:02d}.mp3"
        clip_mp4 = f"{output_dir}/clip_{i:02d}.mp4"
        
        print(f"Encoding clip {i}/10...")
        cmd = [
            ffmpeg, "-y",
            "-loop", "1",
            "-i", slide_png,
            "-i", audio_mp3,
            "-af", "apad=pad_dur=0.6",
            "-c:v", "libx264",
            "-tune", "stillimage",
            "-c:a", "aac",
            "-b:a", "192k",
            "-pix_fmt", "yuv420p",
            "-shortest",
            clip_mp4
        ]
        subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        clist.write(f"file '{clip_mp4}'\n")

print("All 10 clips encoded! Concatenating into final MP4 video...")
cmd_concat = [
    ffmpeg, "-y",
    "-f", "concat",
    "-safe", "0",
    "-i", concat_list_path,
    "-c", "copy",
    final_video
]
subprocess.run(cmd_concat, check=True)

# Also copy to artifact directory
artifact_video = os.path.join(artifact_dir, "bayes_erklaervideo_stichproben_mcmc.mp4")
subprocess.run(["cp", final_video, artifact_video], check=True)

print(f"Successfully created final video:\n- {final_video}\n- {artifact_video}")
