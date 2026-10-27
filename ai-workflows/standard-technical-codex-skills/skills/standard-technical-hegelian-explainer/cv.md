# CV of the user (Shi Hao Ng)

Extracted verbatim from `Shi_Hao_Ng__Resume - v6 [UK - Robotics].pdf` (dated 2026-10-24) with `pdftotext -layout`. Column alignment is from the PDF.

```
Shi Hao Ng
     +44-7436-514602 | shi-hao.ng25@imperial.ac.uk | linkedin.com/in/shi-hao-ng | github.com/Ice-Citron | sienarindustries.com

Education                                                                     Transfer to BEng requested; expected graduation Jun. 2028
 Imperial College London                                                                                                   London, UK
 Master of Engineering: Computing AI/ML, Year 1: Upper Second Class (2:1)                                        Sep. 2025 – Jun. 2029
 Marlborough College Malaysia                                                                                          Johor, Malaysia
 IB Bilingual Diploma: 42/45 (7, 7, 7 in Higher Level subjects)                                            Graduated May 2025
    • IGCSE: 10A* 2A. Best score in Asia for Computer Science; Best in Malaysia for Design Technology, Physics & Chemistry

Experience
 Robotics & Machine Learning Intern                                                                              Jul. 2026 – Sep. 2026
 Vikaso Robotics 4.0                                                                                              Buckinghamshire, UK
    • Built Vikaso’s first camera-intrinsic and camera-to-robot extrinsic calibration pipeline, achieving ∼3 mm initial extrinsic accuracy;
      deployed and tested for 100+ hours on Universal Robots UR5e arm & Intel RealSense camera
    • Built Vikaso’s first configurable perception stack, integrating 3 segmentation models and 4 CAD-based 6D pose-estimation models
      (all open-source) behind a common interface that accepts RGB or RGB-D input and returns masks and 6D poses
    • Built a benchmark harness evaluating integrated models on 6D localization and 2D detection tasks across 10 public datasets.
      Results matched each model’s published Average Recall, which verified the integrations reproduced the papers
 Re-Founder & Co-President                                                                                      Oct. 2025 – Apr. 2026
 Imperial College Drone Society                                                                                      London, UK
    • Re-established Imperial College Drone Society, secured £3.8k sponsorship pledges, 80+ members, ran drone-building events

 Engineering Intern                                                                                              Jun. 2025 – Jul. 2025
 Nosco Asia (S) Pte Ltd                                                                                              Taichung, Taiwan
   • Assisted hot-repair ceramic-welding crews servicing still-running large-scale glass furnaces. Operated welding machinery, performed
      ceramic welding, cut refractory bricks, and learned how glass production lines operate

Projects
 Intrinsic AI for Industry Challenge | PyTorch, LeRobot ACT, Isaac Sim, Gazebo | Website                                     2026
    • Experimented with RL (Isaac Lab), imitation learning (ACT) and scripted FSM policies for fiber-optic cable insertion into
      server-rack ports on a simulated UR5e; placed 49th of 196 teams with Team Quantarune
    • For the final submission, developed a hybrid ACT–FSM policy using ACT (Action Chunking Transformers) for approach and
      cable-to-port alignment, followed by rule-based insertion with force-scaled steps and lateral motion limits. Added automatic
      retraction and realignment to recover from failed attempts
    • Scored 89 on held-out competition evals versus 141 locally.   Later analysis found the local tests did not reflect competition
      conditions, motivating a paper on the policy (in preparation)
 GPT-2 Pre-training: Layer Normalization Study | PyTorch, DDP, HPC | GitHub | Paper                                 2024 – 2025
   • Reproduced GPT-2 124M from scratch in PyTorch, pre-trained on FineWeb using DDP on 4× NVIDIA H100s; tuned GPU
     configurations, batch size, mixed precision and torch.compile, achieving training throughput of 1.27M tokens/s
   • Implemented LayerNorm, RMSNorm & PowerNorm. RMSNorm achieved comparable loss at 6% higher throughput than Layer-
     Norm; PowerNorm’s gradients exploded. Ran normalization ablations and analyzed per-layer gradients
   • Fine-tuned base & ablated models on BillSum and SQuAD. Evaluated summarization and QA using ROUGE/F1 and GPT-4o
     (LLM-as-a-judge). Published weights on Hugging Face; awarded Grade A for this IB Computer Science Extended Essay
 Google EDUtech Asia: Geospatial Waste Analytics | YOLOv11, PyTorch, Fine-tuning, GCP | Website | GitHub                    2024
   • ML Lead: Won 1st place and keynoted to 1k+ people. Built waste analytics platform that maps trash distribution: drone images
     are sent to fine-tuned YOLOv11-L, storing detections in Flask/Firebase backend and plotting heatmap with Google Maps API
   • Collated 300k images from 25 waste datasets and merged 31 label names into 8 classes. Fine-tuned YOLOv11-L for 9 epochs on
     GCP’s NVIDIA H100, reaching 0.381 mAP50 and 0.264 mAP50-95 on eval set; inference runs at ∼15 ms/image on NVIDIA L4
 C++ OpenGL Game Engine | C++17, OpenGL, GLFW, ImGui | GitHub | Website                                                2022 – 2023
   • Recreated game engine in C++/OpenGL following The Cherno’s tutorials: batched sprite renderer with texture slots over shared
     buffers, GLSL shaders for texturing and lighting, and a hand-written matrix library with a transformation stack
   • Built an application framework using GLFW for windows & input and C++ templates to dispatch events to handlers, with a
     layer stack that routes events to overlays first. Integrated ImGui docking and multi-viewport support into the render loop
 Augmented Electromagnetic Railgun | LS-Dyna, Fusion 360, High-Voltage Electronics | Website | Paper                2024 – 2025
   • Iterated through 3 physical prototypes modeled in Fusion 360; ran 70 LS-DYNA simulations across 10 voltage conditions to settle
     on an augmented railgun design. Built a 450 V, 10 mF capacitor bank for the power system; final railgun averaged 132 km/h
     across 10+ firing trials, estimated from high-speed footage
 Custom FPV Racing Drone | Blender, Betaflight, PET-CF 3D Printing, ELRS | Website                                          2025
   • Designed the frame in Blender and 3D-printed in PET-CF; hand-soldered electronics (Mamba F722, DJI O4 Pro, ELRS), tuned
     Betaflight PIDs. Replaced frame due to crash damage; flew the completed 610 g drone in Acro (240 km/h theoretical top speed)

Technical Skills
 Languages: Python, C/C++, Kotlin, Java, Haskell, TypeScript/JavaScript, CUDA, RISC-V assembly
 ML: PyTorch, DDP, Hugging Face Transformers, LeRobot, Weights & Biases, NumPy, vLLM, llama.cpp
 Computer Vision: OpenCV, YOLO, SAM, CNOS, FoundationPose, MegaPose, 6D pose estimation, camera calibration
 Robotics & Simulation: ROS 2, MuJoCo C API, Gazebo, Isaac Sim, Universal Robots UR5e, NVIDIA Cosmos 3
 Systems & Web: Linux/CUDA toolchain, Git, Docker, GCP, AWS, OpenGL, GLFW, React, Node.js, Firebase
 FEA & Hardware: Fusion 360, Blender, Ansys Fluent, LS-Dyna, 3D printing, CNC, high-voltage electronics, Betaflight, soldering
```
