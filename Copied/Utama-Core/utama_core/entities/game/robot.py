from __future__ import annotations

import logging
from dataclasses import dataclass

from utama_core.entities.data.vector import Vector2D

logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class Robot:
    id: int
    is_friendly: bool
    has_ball: bool  # Friendly and enemy now have this, friendly is from IR sensor, enemy from position
    p: Vector2D
    v: Vector2D
    a: Vector2D
    orientation: float
