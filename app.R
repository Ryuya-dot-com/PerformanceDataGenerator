# MFRM一次元性分析 - 全パラメータ制御Shinyアプリ
# カスタム評価尺度とデータダウンロード機能付き

library(shiny)
library(tidyverse)
library(psych)      # 主成分分析用
library(ggplot2)    # 可視化用
library(gridExtra)  # 複数グラフの表示用
library(reshape2)   # データ整形用
library(mvtnorm)    # 多変量正規分布のサンプリング用
library(gt)         # 綺麗な表の出力
library(gtExtras)   # gt拡張機能
library(DT)         # データテーブル表示

# MFRMデータと残差を生成する関数
generate_mfrm_data <- function(
    n_participants = 60,
    n_raters = 4,
    n_tasks = 3,
    n_criteria = 4,
    residual_type = 1, # 1:ランダム, 2:評価者関連, 3:タスク関連
    residual_sd = 0.5, # 残差の標準偏差（小さいほど一次元性が高い）
    within_facet_corr = 0.3, # ファセット内の相関係数（大きいほど多次元性が高まる）
    participant_sd = 1.0, # 受験者能力のばらつき
    rater_sd = 0.2, # 評価者の厳しさのばらつき
    task_sd = 0.2, # タスク難易度のばらつき
    criteria_sd = 0.2, # 評価観点難易度のばらつき
    participant_mean = 0, # 受験者能力の平均
    rater_mean = 0, # 評価者の厳しさの平均
    task_mean = 0, # タスク難易度の平均
    criteria_mean = 0, # 評価観点難易度の平均
    threshold_values = c(-1.2, -0.4, 0.4, 1.2), # 閾値パラメータ
    rating_scale_min = 1, # 評価スケール最小値
    rating_scale_max = 5, # 評価スケール最大値
    participant_distribution = "normal", # 受験者能力の分布
    rater_effect_size = 1.0, # 評価者効果の大きさ
    task_effect_size = 1.0, # タスク効果の大きさ
    criteria_effect_size = 1.0, # 評価観点効果の大きさ
    cross_design = "complete", # "complete", "balanced", "random"のいずれか
    rater_assignment_rate = 0.5, # 各受験者に割り当てられる評価者の割合（cross_design = "random"または"balanced"の場合）
    task_assignment_rate = 1.0, # 各受験者に割り当てられるタスクの割合
    criteria_assignment_rate = 1.0, # 各評価に使用される評価観点の割合
    missing_data_rate = 0.0, # 欠測データの発生率（0〜1）
    balanced_blocks = TRUE, # cross_design = "balanced"の場合、均等なブロックを作成するか
    seed_value = 123
) {
  # 乱数生成のシード値を固定
  set.seed(seed_value)
  
  # 評価者ID
  rater_ids <- LETTERS[1:n_raters]  # A, B, C, D
  
  # タスク名
  task_names <- paste0("Task ", LETTERS[1:n_tasks])
  
  # 評価観点
  criteria_names <- paste0("Criterion ", LETTERS[1:n_criteria])
  
  # クロス設計に基づいてデータセットを作成
  if (cross_design == "complete") {
    # 完全クロス設計：すべての受験者×評価者×タスク×観点の組み合わせ
    speaking_data <- expand.grid(
      participant_id = 1:n_participants,
      rater_id = rater_ids,
      task = task_names,
      criteria = criteria_names)
  } else if (cross_design == "balanced") {
    # バランス設計：受験者を複数のグループに分け、各グループに評価者の一部を割り当てる
    n_rater_per_participant <- max(1, round(n_raters * rater_assignment_rate))
    n_task_per_participant <- max(1, round(n_tasks * task_assignment_rate))
    n_criteria_per_task <- max(1, round(n_criteria * criteria_assignment_rate))
    
    # 受験者をグループに分ける（ブロック設計）
    if (balanced_blocks && n_raters > 1) {
      n_blocks <- ceiling(n_raters / n_rater_per_participant)
      block_size <- ceiling(n_participants / n_blocks)
      
      # 各ブロックの受験者と評価者の割り当てを作成
      assignment_list <- list()
      
      for (block in 1:n_blocks) {
        # このブロックの受験者
        start_idx <- (block - 1) * block_size + 1
        end_idx <- min(block * block_size, n_participants)
        participants_in_block <- start_idx:end_idx
        
        # このブロックの評価者（循環的に割り当て）
        rater_indices <- ((block - 1) * n_rater_per_participant + 1):
          (block * n_rater_per_participant)
        rater_indices <- ((rater_indices - 1) %% n_raters) + 1
        raters_in_block <- rater_ids[rater_indices]
        
        # タスクも同様に割り当て（必要に応じて）
        task_indices <- sample(1:n_tasks, n_task_per_participant)
        tasks_in_block <- task_names[task_indices]
        
        # 評価観点も同様に割り当て
        criteria_indices <- sample(1:n_criteria, n_criteria_per_task)
        criteria_in_block <- criteria_names[criteria_indices]
        
        # このブロックの組み合わせを作成
        block_data <- expand.grid(
          participant_id = participants_in_block,
          rater_id = raters_in_block,
          task = tasks_in_block,
          criteria = criteria_in_block
        )
        
        assignment_list[[block]] <- block_data
      }
      
      # すべてのブロックを結合
      speaking_data <- do.call(rbind, assignment_list)
    } else {
      # 各受験者に評価者とタスクをランダムに割り当て
      assignment_list <- list()
      
      for (p in 1:n_participants) {
        # この受験者の評価者をランダム選択
        selected_raters <- sample(rater_ids, n_rater_per_participant)
        
        # この受験者のタスクをランダム選択
        selected_tasks <- sample(task_names, n_task_per_participant)
        
        # この受験者の評価観点をランダム選択
        selected_criteria <- sample(criteria_names, n_criteria_per_task)
        
        # この受験者の組み合わせを作成
        participant_data <- expand.grid(
          participant_id = p,
          rater_id = selected_raters,
          task = selected_tasks,
          criteria = selected_criteria
        )
        
        assignment_list[[p]] <- participant_data
      }
      
      # すべての受験者のデータを結合
      speaking_data <- do.call(rbind, assignment_list)
    }
  } else if (cross_design == "random") {
    # ランダム設計：各受験者にランダムな評価者とタスクを割り当て
    # 各評価の期待数を計算
    expected_evaluations <- n_participants * n_raters * n_tasks * n_criteria
    expected_raters_per_participant <- max(1, round(n_raters * rater_assignment_rate))
    expected_tasks_per_participant <- max(1, round(n_tasks * task_assignment_rate))
    expected_criteria_per_evaluation <- max(1, round(n_criteria * criteria_assignment_rate))
    
    # 各受験者ごとの評価数
    evaluations_per_participant <- expected_raters_per_participant * 
      expected_tasks_per_participant * 
      expected_criteria_per_evaluation
    
    # 各受験者の評価データを作成
    assignment_list <- list()
    
    for (p in 1:n_participants) {
      # 何人の評価者から評価されるか
      n_raters_for_this_participant <- expected_raters_per_participant
      
      # どの評価者からか
      selected_raters <- sample(rater_ids, n_raters_for_this_participant)
      
      # 何個のタスクを実施するか
      n_tasks_for_this_participant <- expected_tasks_per_participant
      
      # どのタスクを実施するか
      selected_tasks <- sample(task_names, n_tasks_for_this_participant)
      
      # 各タスク×評価者の組み合わせに対して、どの評価観点を使用するか
      participant_records <- list()
      
      for (r in selected_raters) {
        for (t in selected_tasks) {
          # このタスク×評価者の組み合わせでどの評価観点を使用するか
          n_criteria_for_this_eval <- expected_criteria_per_evaluation
          selected_criteria <- sample(criteria_names, n_criteria_for_this_eval)
          
          # 各評価観点のレコードを作成
          task_data <- data.frame(
            participant_id = p,
            rater_id = r,
            task = t,
            criteria = selected_criteria,
            stringsAsFactors = FALSE
          )
          
          participant_records[[length(participant_records) + 1]] <- task_data
        }
      }
      
      # この受験者のすべての記録を結合
      if (length(participant_records) > 0) {
        participant_all_data <- do.call(rbind, participant_records)
        assignment_list[[p]] <- participant_all_data
      }
    }
    
    # すべての受験者のデータを結合
    speaking_data <- do.call(rbind, assignment_list)
  }
  
  # 欠測データの生成
  if (missing_data_rate > 0) {
    # 欠測とするデータの行数を計算
    n_total_rows <- nrow(speaking_data)
    n_missing_rows <- round(n_total_rows * missing_data_rate)
    
    if (n_missing_rows > 0) {
      # ランダムに行を選択して削除
      missing_indices <- sample(1:n_total_rows, n_missing_rows)
      speaking_data <- speaking_data[-missing_indices, ]
    }
  }
  
  # 閾値パラメータを整理
  n_categories <- length(threshold_values) + 1
  if (n_categories != (rating_scale_max - rating_scale_min + 1)) {
    # 評価スケールと閾値の数が一致しない場合は調整
    n_thresholds_needed <- rating_scale_max - rating_scale_min
    if (length(threshold_values) > n_thresholds_needed) {
      # 閾値が多すぎる場合は切り詰める
      threshold_values <- threshold_values[1:n_thresholds_needed]
    } else if (length(threshold_values) < n_thresholds_needed) {
      # 閾値が少なすぎる場合は補間する
      old_range <- range(threshold_values)
      thresholds_to_add <- n_thresholds_needed - length(threshold_values)
      if (thresholds_to_add > 0) {
        new_thresholds <- seq(old_range[1], old_range[2], length.out = n_thresholds_needed)
        threshold_values <- new_thresholds
      }
    }
  }
  # 閾値は必ず昇順に
  threshold_values <- sort(threshold_values)
  
  # 受験者能力（θ）の生成
  if (participant_distribution == "normal") {
    theta <- rnorm(n_participants, mean = participant_mean, sd = participant_sd)
  } else if (participant_distribution == "uniform") {
    range <- participant_sd * sqrt(12) # 同じ分散になるよう調整
    theta <- runif(n_participants, min = participant_mean - range/2, max = participant_mean + range/2)
  } else if (participant_distribution == "bimodal") {
    # 二峰性分布（2つの正規分布を混合）
    group1 <- rnorm(n_participants/2, mean = participant_mean - participant_sd, sd = participant_sd/2)
    group2 <- rnorm(n_participants - n_participants/2, mean = participant_mean + participant_sd, sd = participant_sd/2)
    theta <- c(group1, group2)
  }
  
  # 評価者の厳しさ（α）
  alpha <- rnorm(n_raters, mean = rater_mean, sd = rater_sd * rater_effect_size)
  
  # タスクの難易度（β）
  beta <- rnorm(n_tasks, mean = task_mean, sd = task_sd * task_effect_size)
  
  # 評価観点の難易度（γ）
  gamma <- rnorm(n_criteria, mean = criteria_mean, sd = criteria_sd * criteria_effect_size)
  
  # 能力値、厳しさ、難易度の情報を追加
  speaking_data <- speaking_data %>%
    mutate(
      # 受験者IDから能力値を取得
      ability_index = participant_id,
      ability = theta[ability_index],
      
      # 評価者IDから厳しさを取得
      severity_index = match(rater_id, rater_ids),
      severity = alpha[severity_index],
      
      # タスク名から難易度を取得
      task_index = match(task, task_names),
      task_difficulty = beta[task_index],
      
      # 評価観点から難易度を取得
      criteria_index = match(criteria, criteria_names),
      criteria_difficulty = gamma[criteria_index],
      
      # 理論上のロジット位置を計算
      logit_position = ability - severity - task_difficulty - criteria_difficulty
    )
  
  # カテゴリ確率を計算する関数
  calculate_category_probs <- function(logit_position, thresholds) {
    n_cats <- length(thresholds) + 1
    probs <- numeric(n_cats)
    
    # カテゴリ1の確率
    probs[1] <- 1 / (1 + exp(logit_position - thresholds[1]))
    
    # カテゴリ2からn-1までの確率
    for (k in 2:(n_cats-1)) {
      probs[k] <- 1 / (1 + exp(logit_position - thresholds[k])) - 
        1 / (1 + exp(logit_position - thresholds[k-1]))
    }
    
    # 最後のカテゴリの確率
    probs[n_cats] <- 1 - 1 / (1 + exp(logit_position - thresholds[n_cats-1]))
    
    return(probs)
  }
  
  # 期待値を計算
  speaking_data <- speaking_data %>%
    rowwise() %>%
    mutate(
      # 各カテゴリの確率を計算
      category_probs = list(calculate_category_probs(logit_position, threshold_values)),
      
      # 期待スコア（理論平均値）
      expected_score = sum(seq(from = rating_scale_min, 
                               length.out = length(category_probs)) * category_probs),
      
      # 理論分散
      score_variance = sum((seq(from = rating_scale_min, 
                                length.out = length(category_probs)) - expected_score)^2 * category_probs)
    ) %>%
    ungroup()
  
  # 各項目（評価者×タスク×観点）に一意のIDを付ける
  speaking_data <- speaking_data %>%
    mutate(item_id = paste(rater_id, task, criteria, sep = "_"))
  
  if (residual_type == 1) {
    # 完全にランダムな残差 - 一次元性が高いモデル
    speaking_data$std_residual <- rnorm(nrow(speaking_data), mean = 0, sd = residual_sd)
    
  } else if (residual_type == 2) {
    # 評価者に関連した残差構造
    # 各評価者内の項目間に相関を持たせる
    
    # 相関行列を作成
    n_items <- n_tasks * n_criteria
    rater_corr <- within_facet_corr  # 同じ評価者内の項目間の相関
    
    # 各評価者ごとに相関のある残差を生成
    residuals_list <- list()
    
    for (r in 1:n_raters) {
      # この評価者の相関行列
      corr_matrix <- matrix(rater_corr, nrow = n_items, ncol = n_items)
      diag(corr_matrix) <- 1
      
      # 分散共分散行列
      sigma <- residual_sd^2 * corr_matrix
      
      # 多変量正規分布からサンプリング
      rater_residuals <- rmvnorm(n_participants, mean = rep(0, n_items), sigma = sigma)
      
      # データフレームに変換
      rater_df <- as.data.frame(rater_residuals)
      names(rater_df) <- paste0("item_", 1:n_items)
      rater_df$participant_id <- 1:n_participants
      
      # 長形式に変換
      rater_long <- pivot_longer(
        rater_df, 
        cols = starts_with("item_"),
        names_to = "item_num", 
        values_to = "residual"
      )
      
      # アイテム番号とタスク・評価観点の対応を作成
      item_map <- expand.grid(
        task = 1:n_tasks,
        criteria = 1:n_criteria
      ) %>%
        mutate(item_num = paste0("item_", 1:n_items))
      
      # 残差にタスクと評価観点の情報を追加
      rater_long <- rater_long %>%
        left_join(item_map, by = "item_num") %>%
        mutate(
          rater_id = rater_ids[r],
          task = task_names[task],
          criteria = criteria_names[criteria]
        ) %>%
        select(participant_id, rater_id, task, criteria, residual)
      
      residuals_list[[r]] <- rater_long
    }
    
    # すべての評価者の残差を結合
    all_residuals <- bind_rows(residuals_list)
    
    # 元のデータに残差を結合
    speaking_data <- speaking_data %>%
      left_join(
        all_residuals,
        by = c("participant_id", "rater_id", "task", "criteria")
      ) %>%
      rename(std_residual = residual)
    
  } else if (residual_type == 3) {
    # タスクに関連した残差構造
    # 各タスク内の項目間に相関を持たせる
    
    # 相関行列を作成
    n_items_per_task <- n_raters * n_criteria
    task_corr <- within_facet_corr  # 同じタスク内の項目間の相関
    
    # 各タスクごとに相関のある残差を生成
    residuals_list <- list()
    
    for (t in 1:n_tasks) {
      # このタスクの相関行列
      corr_matrix <- matrix(task_corr, nrow = n_items_per_task, ncol = n_items_per_task)
      diag(corr_matrix) <- 1
      
      # 分散共分散行列
      sigma <- residual_sd^2 * corr_matrix
      
      # 多変量正規分布からサンプリング
      task_residuals <- rmvnorm(n_participants, mean = rep(0, n_items_per_task), sigma = sigma)
      
      # データフレームに変換
      task_df <- as.data.frame(task_residuals)
      names(task_df) <- paste0("item_", 1:n_items_per_task)
      task_df$participant_id <- 1:n_participants
      
      # 長形式に変換
      task_long <- pivot_longer(
        task_df, 
        cols = starts_with("item_"),
        names_to = "item_num", 
        values_to = "residual"
      )
      
      # アイテム番号と評価者・評価観点の対応を作成
      item_map <- expand.grid(
        rater = 1:n_raters,
        criteria = 1:n_criteria
      ) %>%
        mutate(item_num = paste0("item_", 1:n_items_per_task))
      
      # 残差に評価者と評価観点の情報を追加
      task_long <- task_long %>%
        left_join(item_map, by = "item_num") %>%
        mutate(
          rater_id = rater_ids[rater],
          task = task_names[t],
          criteria = criteria_names[criteria]
        ) %>%
        select(participant_id, rater_id, task, criteria, residual)
      
      residuals_list[[t]] <- task_long
    }
    
    # すべてのタスクの残差を結合
    all_residuals <- bind_rows(residuals_list)
    
    # 元のデータに残差を結合
    speaking_data <- speaking_data %>%
      left_join(
        all_residuals,
        by = c("participant_id", "rater_id", "task", "criteria")
      ) %>%
      rename(std_residual = residual)
  }
  
  # 標準残差から観測スコアを計算
  speaking_data <- speaking_data %>%
    mutate(
      # 最初に標準残差から生スコアを生成
      raw_residual = std_residual * sqrt(score_variance),
      score_float = expected_score + raw_residual,
      score_float = pmin(pmax(score_float, rating_scale_min), rating_scale_max),
      score = round(score_float)
    ) %>%
    rowwise() %>%
    mutate(
      # スコア作成後、モデル残差を計算
      raw_residual = score - expected_score,
      std_residual = raw_residual / sqrt(score_variance),
      std_residual_sq = std_residual^2
    ) %>%
    ungroup()
  
  # メタデータを含む結果リストを返す
  return(list(
    data = speaking_data,
    metadata = list(
      n_participants = n_participants,
      n_raters = n_raters,
      n_tasks = n_tasks,
      n_criteria = n_criteria,
      residual_type = residual_type,
      rating_scale = c(rating_scale_min, rating_scale_max),
      thresholds = threshold_values,
      participant_distribution = participant_distribution
    )
  ))
}

# 残差PCA分析を実行する関数
run_pca_analysis <- function(speaking_data) {
  # 項目IDからファセット情報を抽出
  item_facet_info <- speaking_data %>%
    select(item_id, rater_id, task, criteria) %>%
    distinct()
  
  # ワイド形式の残差行列を作成（受験者×項目）
  residual_matrix <- speaking_data %>%
    select(participant_id, item_id, std_residual) %>%
    pivot_wider(
      id_cols = participant_id,
      names_from = item_id,
      values_from = std_residual
    ) %>%
    column_to_rownames("participant_id")
  
  # 欠測値の存在をチェック
  has_missing <- any(is.na(residual_matrix))
  
  if (has_missing) {
    # 欠測値がある場合は相関行列を直接計算（ペアワイズ法）
    correlation_matrix <- cor(residual_matrix, use = "pairwise.complete.obs")
    
    # NAや無限値を0に置き換え（PCAの安定性のため）
    correlation_matrix[is.na(correlation_matrix)] <- 0
    correlation_matrix[is.infinite(correlation_matrix)] <- 0
    
    # 対角成分を1に設定
    diag(correlation_matrix) <- 1
    
    # 固有値分解を直接実行
    eigen_result <- eigen(correlation_matrix)
    eigenvalues <- eigen_result$values
    loadings <- eigen_result$vectors
    
    # 固有値が微小な負の値になることがあるため、ゼロ以下の値を微小な正の値に置き換え
    eigenvalues[eigenvalues <= 0] <- 1e-10
    
    # 分散説明率を計算
    var_explained <- eigenvalues / sum(eigenvalues) * 100
    cum_var <- cumsum(var_explained)
    
    # 結果のデータフレーム
    max_components <- length(eigenvalues)
    pca_results <- data.frame(
      PC = 1:max_components,
      Variance = var_explained,
      Cumulative = cum_var
    )
    
    # 固有値1以上の主成分数
    eigenvalues_gt_1 <- sum(eigenvalues > 1)
    
    # 手動でオブジェクトを作成
    pca_results_obj <- list(
      values = eigenvalues,
      loadings = loadings,
      Vaccounted = rbind(
        eigenvalues, 
        var_explained/100,  # proportion of variance
        cum_var/100         # cumulative proportion
      )
    )
    rownames(pca_results_obj$Vaccounted) <- c("Eigenvalues", "Proportion Var", "Cumulative Var")
    colnames(pca_results_obj$Vaccounted) <- paste0("PC", 1:max_components)
    
  } else {
    # 欠測値がない場合は通常のPCA
    # 全ての可能な主成分を抽出（最大は項目数または受験者数-1のいずれか小さい方）
    max_components <- min(nrow(residual_matrix) - 1, ncol(residual_matrix))
    
    # 全データの主成分分析
    pca_results_obj <- principal(residual_matrix, nfactors = max_components, rotate = "none")
    
    # 分散説明率を抽出（全主成分）
    var_explained <- pca_results_obj$Vaccounted[2, ] * 100  # パーセンテージに変換
    cum_var <- pca_results_obj$Vaccounted[3, ] * 100        # 累積分散説明率
    
    # 結果のデータフレーム
    pca_results <- data.frame(
      PC = 1:max_components,
      Variance = var_explained,
      Cumulative = cum_var
    )
    
    # 固有値を抽出
    eigenvalues <- pca_results_obj$values[1:max_components]
    
    # 固有値1以上の主成分数をカウント
    eigenvalues_gt_1 <- sum(eigenvalues > 1)
  }
  
  # 一次元性評価のためのデータを保存
  dimensionality_assessment <- list(
    pca_results = pca_results,
    eigenvalues = eigenvalues,
    eigenvalues_gt_1 = eigenvalues_gt_1,
    var_explained_first_pc = var_explained[1],
    cum_var_explained_5pc = ifelse(length(cum_var) >= 5, cum_var[5], max(cum_var)),
    loadings = if (has_missing) loadings else pca_results_obj$loadings
  )
  
  return(list(
    speaking_data = speaking_data,
    residual_matrix = residual_matrix,
    item_facet_info = item_facet_info,
    dimensionality = dimensionality_assessment
  ))
}

# スクリープロットを作成する関数
create_scree_plot <- function(eigenvalues, title = "スクリープロット") {
  display_components <- min(15, length(eigenvalues))
  
  plot_data <- data.frame(
    PC = 1:display_components,
    Eigenvalue = eigenvalues[1:display_components]
  )
  
  p <- ggplot(plot_data, aes(x = PC, y = Eigenvalue)) +
    geom_line(linewidth = 1, color = "blue") +
    geom_point(size = 3, color = "blue") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(
      title = title,
      x = "主成分",
      y = "固有値"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(size = 14)
    )
  
  return(p)
}

# 分散説明率プロットを作成する関数
create_var_explained_plot <- function(var_explained, title = "分散説明率") {
  display_components <- min(10, length(var_explained))
  
  plot_data <- data.frame(
    PC = 1:display_components,
    VarExplained = var_explained[1:display_components]
  )
  
  p <- ggplot(plot_data, aes(x = PC, y = VarExplained)) +
    geom_col(fill = "steelblue") +
    labs(
      title = title,
      x = "主成分",
      y = "分散説明率 (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(size = 14)
    )
  
  return(p)
}

# 一次元性評価情報を作成する関数
create_dimensionality_info <- function(dim_assessment) {
  first_eigenvalue <- dim_assessment$eigenvalues[1]
  eigenvalues_gt_1 <- dim_assessment$eigenvalues_gt_1
  var_explained_1pc <- dim_assessment$var_explained_first_pc
  cum_var_5pc <- dim_assessment$cum_var_explained_5pc
  
  # 一次元性の評価
  unidimensionality <- if (var_explained_1pc >= 20 && eigenvalues_gt_1 <= 2) {
    "強い一次元性"
  } else if (var_explained_1pc >= 15 && eigenvalues_gt_1 <= 3) {
    "中程度の一次元性"
  } else {
    "弱い一次元性または多次元"
  }
  
  # テーブルデータ作成
  table_data <- data.frame(
    Measure = c(
      "第1固有値",
      "固有値>1の主成分数",
      "第1主成分分散説明率(%)",
      "上位5主成分累積説明率(%)",
      "一次元性評価"
    ),
    Value = c(
      format(round(first_eigenvalue, 2), nsmall = 2),
      eigenvalues_gt_1,
      format(round(var_explained_1pc, 2), nsmall = 2),
      format(round(cum_var_5pc, 2), nsmall = 2),
      unidimensionality
    )
  )
  
  return(list(
    unidimensionality = unidimensionality,
    table_data = table_data
  ))
}

# 閾値の動的な生成
generate_thresholds <- function(rating_min, rating_max) {
  n_categories <- rating_max - rating_min + 1
  n_thresholds <- n_categories - 1
  
  if (n_thresholds <= 0) return(numeric(0))
  
  # 範囲を-2.0から2.0に設定して均等配置
  thresholds <- seq(-2.0, 2.0, length.out = n_thresholds)
  return(thresholds)
}

# データセットの詳細な構造を分析する関数
analyze_dataset_structure <- function(speaking_data) {
  # 各受験者のデータ数
  participant_summary <- speaking_data %>%
    group_by(participant_id) %>%
    summarize(
      n_evaluations = n(),
      n_raters = n_distinct(rater_id),
      n_tasks = n_distinct(task),
      n_criteria = n_distinct(criteria),
      .groups = "drop"
    ) %>%
    summarize(
      total_participants = n(),
      mean_evaluations = mean(n_evaluations),
      min_evaluations = min(n_evaluations),
      max_evaluations = max(n_evaluations),
      mean_raters = mean(n_raters),
      min_raters = min(n_raters),
      max_raters = max(n_raters),
      mean_tasks = mean(n_tasks),
      min_tasks = min(n_tasks),
      max_tasks = max(n_tasks),
      mean_criteria = mean(n_criteria),
      min_criteria = min(n_criteria),
      max_criteria = max(n_criteria),
      .groups = "drop"
    )
  
  # 各評価者のデータ数
  rater_summary <- speaking_data %>%
    group_by(rater_id) %>%
    summarize(
      n_evaluations = n(),
      n_participants = n_distinct(participant_id),
      n_tasks = n_distinct(task),
      n_criteria = n_distinct(criteria),
      .groups = "drop"
    ) %>%
    summarize(
      total_raters = n(),
      mean_evaluations = mean(n_evaluations),
      min_evaluations = min(n_evaluations),
      max_evaluations = max(n_evaluations),
      mean_participants = mean(n_participants),
      min_participants = min(n_participants),
      max_participants = max(n_participants),
      .groups = "drop"
    )
  
  # データセット全体の情報
  dataset_info <- list(
    total_evaluations = nrow(speaking_data),
    sparsity = 1 - (nrow(speaking_data) / 
                      (n_distinct(speaking_data$participant_id) * 
                         n_distinct(speaking_data$rater_id) * 
                         n_distinct(speaking_data$task) * 
                         n_distinct(speaking_data$criteria))),
    connectivity = check_dataset_connectivity(speaking_data)
  )
  
  # 結果を返す
  return(list(
    participant_summary = participant_summary,
    rater_summary = rater_summary,
    dataset_info = dataset_info
  ))
}

# データセットの連結性（connectivity）をチェックする関数
check_dataset_connectivity <- function(speaking_data) {
  # 受験者×評価者の接続性マトリックス
  participant_rater_matrix <- with(speaking_data, table(participant_id, rater_id))
  
  # 受験者×タスクの接続性マトリックス
  participant_task_matrix <- with(speaking_data, table(participant_id, task))
  
  # 評価者×タスクの接続性マトリックス
  rater_task_matrix <- with(speaking_data, table(rater_id, task))
  
  # 受験者間の接続
  participant_connected <- all(rowSums(participant_rater_matrix) > 0)
  
  # 評価者間の接続
  rater_connected <- all(colSums(participant_rater_matrix) > 0)
  
  # データセット分割の確認
  # 注: 完全な連結性チェックにはもっと複雑なグラフ理論アルゴリズムが必要
  # ここでは簡易的なチェックのみ実装
  
  result <- list(
    all_participants_evaluated = participant_connected,
    all_raters_used = rater_connected,
    min_raters_per_participant = min(rowSums(participant_rater_matrix > 0)),
    max_raters_per_participant = max(rowSums(participant_rater_matrix > 0)),
    min_participants_per_rater = min(colSums(participant_rater_matrix > 0)),
    max_participants_per_rater = max(colSums(participant_rater_matrix > 0)),
    sufficient_connectivity = min(rowSums(participant_rater_matrix > 0)) >= 1 && 
      min(colSums(participant_rater_matrix > 0)) >= 1
  )
  
  return(result)
}

# Fit統計量を計算する関数
calculate_fit_statistics <- function(speaking_data) {
  # 項目レベルの残差を計算
  speaking_data <- speaking_data %>%
    rowwise() %>%
    mutate(
      raw_residual = score - expected_score,
      std_residual_sq = std_residual^2
    ) %>%
    ungroup()
  
  # 参加者のfit統計量
  participant_fit <- speaking_data %>%
    group_by(participant_id) %>%
    summarize(
      infit = sum(std_residual_sq * score_variance) / sum(score_variance),
      outfit = mean(std_residual_sq),
      n_observations = n(),
      .groups = "drop"
    ) %>%
    mutate(
      infit_t = calculate_t_statistic(infit, n_observations),
      outfit_t = calculate_t_statistic(outfit, n_observations)
    )
  
  # 評価者のfit統計量
  rater_fit <- speaking_data %>%
    group_by(rater_id) %>%
    summarize(
      infit = sum(std_residual_sq * score_variance) / sum(score_variance),
      outfit = mean(std_residual_sq),
      n_observations = n(),
      .groups = "drop"
    ) %>%
    mutate(
      infit_t = calculate_t_statistic(infit, n_observations),
      outfit_t = calculate_t_statistic(outfit, n_observations)
    )
  
  # タスクのfit統計量
  task_fit <- speaking_data %>%
    group_by(task) %>%
    summarize(
      infit = sum(std_residual_sq * score_variance) / sum(score_variance),
      outfit = mean(std_residual_sq),
      n_observations = n(),
      .groups = "drop"
    ) %>%
    mutate(
      infit_t = calculate_t_statistic(infit, n_observations),
      outfit_t = calculate_t_statistic(outfit, n_observations)
    )
  
  # 評価観点のfit統計量
  criteria_fit <- speaking_data %>%
    group_by(criteria) %>%
    summarize(
      infit = sum(std_residual_sq * score_variance) / sum(score_variance),
      outfit = mean(std_residual_sq),
      n_observations = n(),
      .groups = "drop"
    ) %>%
    mutate(
      infit_t = calculate_t_statistic(infit, n_observations),
      outfit_t = calculate_t_statistic(outfit, n_observations)
    )
  
  return(list(
    participant = participant_fit,
    rater = rater_fit,
    task = task_fit,
    criteria = criteria_fit
  ))
}

# t統計量への変換関数
calculate_t_statistic <- function(mnsq, df) {
  # より安定したt統計量計算
  # データが多いときはより控えめな変換を適用
  effective_df <- pmin(df, 30)  
  z <- (mnsq^(1/3) - 1 + (mnsq^(1/3) - 1)^2/3) * 3 / sqrt(2/(9*effective_df))
  
  return(z)
}

# Pathway Map（Fit統計量プロット）を作成する関数
create_pathway_map <- function(fit_stats, facet_name) {
  if (facet_name == "participant") {
    data <- fit_stats$participant
    x_var <- "participant_id"
    title <- "受験者Pathway Map"
  } else if (facet_name == "rater") {
    data <- fit_stats$rater
    x_var <- "rater_id"
    title <- "評価者Pathway Map"
  } else if (facet_name == "task") {
    data <- fit_stats$task
    x_var <- "task"
    title <- "タスクPathway Map"
  } else if (facet_name == "criteria") {
    data <- fit_stats$criteria
    x_var <- "criteria"
    title <- "評価観点Pathway Map"
  }
  
  # 色分け用の判定を追加
  data <- data %>%
    mutate(fit_category = case_when(
      infit_t < -2 ~ "overfit",     # 過適合
      infit_t > 2 ~ "underfit",     # 不適合
      TRUE ~ "acceptable"           # 適合
    ))
  
  p <- ggplot(data, aes(x = .data[["infit_t"]], y = .data[[x_var]], color = .data[["fit_category"]])) +
    geom_point(size = 3) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = c(-2, 2), linetype = "dotted", color = "gray") +
    # 色とラベルの正しい対応関係を設定
    scale_color_manual(
      values = c(
        "acceptable" = "green",   # 適合 - 緑
        "overfit" = "blue",       # 過適合 - 青
        "underfit" = "red"        # 不適合 - 赤
      ),
      labels = c(
        "acceptable" = "適合",
        "overfit" = "過適合",
        "underfit" = "不適合"
      ),
      # データに存在しないカテゴリも凡例に表示
      drop = FALSE
    ) +
    labs(
      title = title,
      x = "Infit t統計量",
      y = "",
      color = "適合度"
    ) +
    theme_minimal()
  
  return(p)
}

# Wright Mapを作成する関数
create_wright_map <- function(speaking_data, facet_params) {
  # 能力値と難易度のパラメータを抽出
  participant_data <- data.frame(
    id = 1:length(facet_params$theta),
    measure = facet_params$theta,
    facet_type = "受験者"
  )
  
  rater_data <- data.frame(
    id = facet_params$rater_ids,
    measure = facet_params$alpha,
    facet_type = "評価者"
  )
  
  task_data <- data.frame(
    id = facet_params$task_names,
    measure = facet_params$beta,
    facet_type = "タスク"
  )
  
  criteria_data <- data.frame(
    id = facet_params$criteria_names,
    measure = facet_params$gamma,
    facet_type = "評価観点"
  )
  
  threshold_data <- data.frame(
    id = paste0("閾値", 1:length(facet_params$threshold_values)),
    measure = facet_params$threshold_values,
    facet_type = "閾値"
  )
  
  # すべてのデータを結合
  all_facets <- rbind(
    rater_data,
    task_data,
    criteria_data,
    threshold_data
  )
  
  # 共通の軸範囲を計算
  x_range <- range(c(participant_data$measure, all_facets$measure))
  x_range <- c(x_range[1] - 1, x_range[2] + 1)
  
  # Wright Mapの作成
  p <- ggplot() +
    # 受験者分布（上部に）
    geom_density(data = participant_data, aes(x = measure),
                 fill = "skyblue", alpha = 0.5) +
    # ファセットのポイント（下部に）
    geom_point(data = all_facets, aes(x = measure, y = reorder(id, measure), color = facet_type), size = 3) +
    geom_text(data = all_facets, aes(x = measure, y = reorder(id, measure), 
                                     label = sprintf("%.2f", measure)), hjust = -0.3, size = 3) +
    # ファセットで分割
    facet_grid(facet_type ~ ., scales = "free_y", space = "free_y") +
    # 範囲を統一
    scale_x_continuous(limits = x_range) +
    # ラベルとスタイル
    labs(
      title = "多相ラッシュモデル Wright Map",
      x = "能力値/難易度（ロジット）",
      y = ""
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "lightgray"),
      strip.text = element_text(face = "bold")
    )
  
  return(p)
}

# カテゴリ確率曲線を作成する関数
create_category_curves <- function(thresholds, rating_scale_min) {
  # 能力値の範囲
  ability_range <- seq(-6, 6, by = 0.1)
  
  # 各カテゴリの確率を計算
  n_categories <- length(thresholds) + 1
  category_probs <- matrix(0, nrow = length(ability_range), ncol = n_categories)
  
  for (i in 1:length(ability_range)) {
    # カテゴリ1の確率
    logit <- ability_range[i]
    category_probs[i, 1] <- 1 / (1 + exp(logit - thresholds[1]))
    
    # カテゴリ2からn-1までの確率
    for (k in 2:(n_categories-1)) {
      category_probs[i, k] <- 1 / (1 + exp(logit - thresholds[k])) - 
        1 / (1 + exp(logit - thresholds[k-1]))
    }
    
    # 最後のカテゴリの確率
    category_probs[i, n_categories] <- 1 - 1 / (1 + exp(logit - thresholds[n_categories-1]))
  }
  
  # データフレームに変換
  plot_data <- data.frame(
    ability = rep(ability_range, n_categories),
    category = rep(seq(rating_scale_min, rating_scale_min + n_categories - 1), 
                   each = length(ability_range)),
    probability = as.vector(category_probs)
  )
  
  # カテゴリ確率曲線プロット - sizeをlinewidthに変更
  p <- ggplot(plot_data, aes(x = ability, y = probability, color = as.factor(category))) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = thresholds, linetype = "dashed", color = "gray") +
    labs(
      title = "カテゴリ確率曲線",
      x = "能力値（ロジット）",
      y = "確率",
      color = "カテゴリ"
    ) +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 1))
  
  return(p)
}

# 相関行列プロットを作成する関数
create_correlation_plot <- function(speaking_data) {
  # ワイド形式の残差行列を作成（受験者×項目）
  residual_matrix <- speaking_data %>%
    select(participant_id, item_id, std_residual) %>%
    pivot_wider(
      id_cols = participant_id,
      names_from = item_id,
      values_from = std_residual
    ) %>%
    select(-participant_id)
  
  # 相関行列を計算
  corr_matrix <- cor(residual_matrix, use = "pairwise.complete.obs")
  
  # 溶かした形式に変換
  corr_data <- reshape2::melt(corr_matrix)
  
  # 相関ヒートマップ
  p <- ggplot(corr_data, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "blue", 
      mid = "white", 
      high = "red", 
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    labs(
      title = "残差の相関ヒートマップ",
      x = "",
      y = "",
      fill = "相関"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
      axis.text.y = element_text(size = 6)
    )
  
  return(p)
}

# Separation、Strata、Reliabilityを計算する関数
calculate_reliability_stats <- function(facet_params, speaking_data) {
  # 1. 受験者の能力(theta)のReliability
  n_participants <- length(facet_params$theta)
  
  # 受験者ごとのスコア合計と平均を計算
  participant_scores <- speaking_data %>%
    group_by(participant_id) %>%
    summarize(
      total_score = sum(score),
      mean_score = mean(score),
      n_items = n(),
      .groups = "drop"
    )
  
  # 受験者能力の分散
  theta_var <- var(facet_params$theta)
  
  # 受験者ごとの測定誤差の分散を計算
  participant_mse <- speaking_data %>%
    group_by(participant_id) %>%
    summarize(
      mse = mean(score_variance),  # score_varianceは各評価の理論分散
      .groups = "drop"
    )
  
  # 平均測定誤差分散
  mean_error_var <- mean(participant_mse$mse)
  
  # Separation, Strata, Reliability計算（受験者）
  participant_separation <- sqrt(theta_var / mean_error_var)
  participant_strata <- (4 * participant_separation + 1) / 3
  participant_reliability <- theta_var / (theta_var + mean_error_var)
  
  # 2. 評価者の厳しさ(alpha)のReliability
  n_raters <- length(facet_params$alpha)
  alpha_var <- var(facet_params$alpha)
  
  # 評価者ごとの測定誤差の分散を概算
  # 評価者ごとの評価数に基づいて誤差分散を推定
  rater_evaluations <- speaking_data %>%
    group_by(rater_id) %>%
    summarize(
      n_evaluations = n(),
      .groups = "drop"
    )
  
  # 評価者の測定誤差分散を評価数の逆数で近似
  mean_rater_error_var <- mean(1 / rater_evaluations$n_evaluations)
  
  # Separation, Strata, Reliability計算（評価者）
  rater_separation <- sqrt(alpha_var / mean_rater_error_var)
  rater_strata <- (4 * rater_separation + 1) / 3
  rater_reliability <- alpha_var / (alpha_var + mean_rater_error_var)
  
  # 3. タスクの難易度(beta)のReliability
  n_tasks <- length(facet_params$beta)
  beta_var <- var(facet_params$beta)
  
  # タスクごとの測定誤差の分散を概算
  task_evaluations <- speaking_data %>%
    group_by(task) %>%
    summarize(
      n_evaluations = n(),
      .groups = "drop"
    )
  
  mean_task_error_var <- mean(1 / task_evaluations$n_evaluations)
  
  # Separation, Strata, Reliability計算（タスク）
  task_separation <- sqrt(beta_var / mean_task_error_var)
  task_strata <- (4 * task_separation + 1) / 3
  task_reliability <- beta_var / (beta_var + mean_task_error_var)
  
  # 4. 評価観点の難易度(gamma)のReliability
  n_criteria <- length(facet_params$gamma)
  gamma_var <- var(facet_params$gamma)
  
  # 評価観点ごとの測定誤差の分散を概算
  criteria_evaluations <- speaking_data %>%
    group_by(criteria) %>%
    summarize(
      n_evaluations = n(),
      .groups = "drop"
    )
  
  mean_criteria_error_var <- mean(1 / criteria_evaluations$n_evaluations)
  
  # Separation, Strata, Reliability計算（評価観点）
  criteria_separation <- sqrt(gamma_var / mean_criteria_error_var)
  criteria_strata <- (4 * criteria_separation + 1) / 3
  criteria_reliability <- gamma_var / (gamma_var + mean_criteria_error_var)
  
  # 結果を返す
  return(list(
    participant = list(
      n = n_participants,
      separation = participant_separation,
      strata = participant_strata,
      reliability = participant_reliability,
      variance = theta_var,
      error_variance = mean_error_var
    ),
    rater = list(
      n = n_raters,
      separation = rater_separation,
      strata = rater_strata,
      reliability = rater_reliability,
      variance = alpha_var,
      error_variance = mean_rater_error_var
    ),
    task = list(
      n = n_tasks,
      separation = task_separation,
      strata = task_strata,
      reliability = task_reliability,
      variance = beta_var,
      error_variance = mean_task_error_var
    ),
    criteria = list(
      n = n_criteria,
      separation = criteria_separation,
      strata = criteria_strata,
      reliability = criteria_reliability,
      variance = gamma_var,
      error_variance = mean_criteria_error_var
    )
  ))
}

# Reliabilityの結果をテーブルとして整形する関数
format_reliability_table <- function(reliability_stats) {
  # 各ファセットのReliability統計をデータフレームに変換
  reliability_df <- data.frame(
    "ファセット" = c("受験者", "評価者", "タスク", "評価観点"),
    "数" = c(
      reliability_stats$participant$n,
      reliability_stats$rater$n,
      reliability_stats$task$n,
      reliability_stats$criteria$n
    ),
    "Separation" = c(
      round(reliability_stats$participant$separation, 2),
      round(reliability_stats$rater$separation, 2),
      round(reliability_stats$task$separation, 2),
      round(reliability_stats$criteria$separation, 2)
    ),
    "Strata" = c(
      round(reliability_stats$participant$strata, 2),
      round(reliability_stats$rater$strata, 2),
      round(reliability_stats$task$strata, 2),
      round(reliability_stats$criteria$strata, 2)
    ),
    "Reliability" = c(
      round(reliability_stats$participant$reliability, 3),
      round(reliability_stats$rater$reliability, 3),
      round(reliability_stats$task$reliability, 3),
      round(reliability_stats$criteria$reliability, 3)
    ),
    "分散" = c(
      round(reliability_stats$participant$variance, 3),
      round(reliability_stats$rater$variance, 3),
      round(reliability_stats$task$variance, 3),
      round(reliability_stats$criteria$variance, 3)
    ),
    "誤差分散" = c(
      round(reliability_stats$participant$error_variance, 3),
      round(reliability_stats$rater$error_variance, 3),
      round(reliability_stats$task$error_variance, 3),
      round(reliability_stats$criteria$error_variance, 3)
    )
  )
  
  return(reliability_df)
}

# Reliability統計を視覚化する関数
create_reliability_plot <- function(reliability_stats) {
  # データをロング形式に変換
  plot_data <- data.frame(
    Facet = c("受験者", "評価者", "タスク", "評価観点"),
    Separation = c(
      reliability_stats$participant$separation,
      reliability_stats$rater$separation,
      reliability_stats$task$separation,
      reliability_stats$criteria$separation
    ),
    Reliability = c(
      reliability_stats$participant$reliability,
      reliability_stats$rater$reliability,
      reliability_stats$task$reliability,
      reliability_stats$criteria$reliability
    )
  )
  
  # Separationのカットオフ値（解釈用）
  separation_cutoffs <- data.frame(
    level = c("低", "中", "高"),
    min_value = c(0, 1.5, 3),
    max_value = c(1.5, 3, Inf)
  )
  
  # Reliabilityのカットオフ値（解釈用）
  reliability_cutoffs <- data.frame(
    level = c("低", "中", "高"),
    min_value = c(0, 0.7, 0.9),
    max_value = c(0.7, 0.9, 1)
  )
  
  # 並び替え順（受験者を最初に表示）
  plot_data$Facet <- factor(plot_data$Facet, 
                            levels = c("受験者", "評価者", "タスク", "評価観点"))
  
  # Separationの棒グラフを作成
  p1 <- ggplot(plot_data, aes(x = Facet, y = Separation, fill = Facet)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%.2f", Separation)), vjust = -0.5, size = 4) +
    # 解釈のための背景色
    annotate("rect", xmin = 0.5, xmax = 4.5, ymin = separation_cutoffs$min_value[1], 
             ymax = separation_cutoffs$max_value[1], fill = "red", alpha = 0.1) +
    annotate("rect", xmin = 0.5, xmax = 4.5, ymin = separation_cutoffs$min_value[2], 
             ymax = separation_cutoffs$max_value[2], fill = "yellow", alpha = 0.1) +
    annotate("rect", xmin = 0.5, xmax = 4.5, ymin = separation_cutoffs$min_value[3], 
             ymax = separation_cutoffs$max_value[3], fill = "green", alpha = 0.1) +
    labs(
      title = "ファセット別Separation指標",
      x = "",
      y = "Separation"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      legend.position = "none",
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    ) +
    scale_fill_brewer(palette = "Set1") +
    ylim(0, max(plot_data$Separation) * 1.2)
  
  # Reliabilityの棒グラフを作成
  p2 <- ggplot(plot_data, aes(x = Facet, y = Reliability, fill = Facet)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%.3f", Reliability)), vjust = -0.5, size = 4) +
    # 解釈のための背景色
    annotate("rect", xmin = 0.5, xmax = 4.5, ymin = reliability_cutoffs$min_value[1], 
             ymax = reliability_cutoffs$max_value[1], fill = "red", alpha = 0.1) +
    annotate("rect", xmin = 0.5, xmax = 4.5, ymin = reliability_cutoffs$min_value[2], 
             ymax = reliability_cutoffs$max_value[2], fill = "yellow", alpha = 0.1) +
    annotate("rect", xmin = 0.5, xmax = 4.5, ymin = reliability_cutoffs$min_value[3], 
             ymax = reliability_cutoffs$max_value[3], fill = "green", alpha = 0.1) +
    labs(
      title = "ファセット別Reliability指標",
      x = "",
      y = "Reliability"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      legend.position = "none",
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    ) +
    scale_fill_brewer(palette = "Set1") +
    ylim(0, 1)
  
  # 2つのグラフを並べて表示
  grid.arrange(p1, p2, ncol = 2)
}

# Shinyアプリケーション
ui <- fluidPage(
  titlePanel("多相ラッシュモデル(MFRM)の一次元性分析 - 全パラメータ制御"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      tabsetPanel(
        # 基本設定タブ
        tabPanel("基本設定",
                 # データ設定パネル
                 wellPanel(
                   h4("クロス設計オプション"),
                   radioButtons("cross_design", "クロス設計タイプ:",
                                choices = list(
                                  "完全クロス設計（すべての組み合わせ）" = "complete",
                                  "バランス設計（均等なブロック割り当て）" = "balanced",
                                  "ランダム設計（ランダムな割り当て）" = "random"
                                ),
                                selected = "complete"),
                   
                   # 条件付きUI
                   conditionalPanel(
                     condition = "input.cross_design != 'complete'",
                     sliderInput("rater_assignment_rate", "各受験者に割り当てる評価者の割合:",
                                 min = 0.1, max = 1.0, value = 0.5, step = 0.1),
                     sliderInput("task_assignment_rate", "各受験者に割り当てるタスクの割合:",
                                 min = 0.1, max = 1.0, value = 1.0, step = 0.1),
                     sliderInput("criteria_assignment_rate", "各評価に使用する評価観点の割合:",
                                 min = 0.1, max = 1.0, value = 1.0, step = 0.1)
                   ),
                   
                   # バランス設計のオプション
                   conditionalPanel(
                     condition = "input.cross_design == 'balanced'",
                     checkboxInput("balanced_blocks", "均等なブロックを作成", value = TRUE)
                   ),
                   
                   # 欠測データのオプション
                   sliderInput("missing_data_rate", "欠測データの発生率:",
                               min = 0.0, max = 0.5, value = 0.0, step = 0.05),
                   
                   # 乱数シード
                   numericInput("seed_value", "乱数シード値:", 123, min = 1, max = 9999)
                 ),
                 
                 wellPanel(
                   h4("データ構造設定"),
                   numericInput("n_participants", "受験者数:", 60, min = 20, max = 500),
                   numericInput("n_raters", "評価者数:", 4, min = 2, max = 20),
                   numericInput("n_tasks", "タスク数:", 3, min = 2, max = 10),
                   numericInput("n_criteria", "評価観点数:", 4, min = 2, max = 10)
                 ),
                 
                 # 評価スケール設定
                 wellPanel(
                   h4("評価スケール設定"),
                   numericInput("rating_scale_min", "評価スケール最小値:", 1, min = 0, max = 10),
                   numericInput("rating_scale_max", "評価スケール最大値:", 5, min = 1, max = 20),
                   checkboxInput("auto_generate_thresholds", "閾値を自動生成", value = TRUE),
                   actionButton("update_thresholds", "閾値を更新", class = "btn-info", width = "100%")
                 ),
                 
                 # 残差構造パネル
                 wellPanel(
                   h4("残差構造"),
                   radioButtons("residual_type", "残差構造タイプ:",
                                choices = list(
                                  "ランダム残差" = 1,
                                  "評価者関連残差" = 2,
                                  "タスク関連残差" = 3
                                ),
                                selected = 1),
                   sliderInput("residual_sd", "残差の標準偏差:",
                               min = 0.01, max = 6.0, value = 0.5, step = 0.01),
                   sliderInput("within_facet_corr", "ファセット内の相関:",
                               min = 0.0, max = 0.9, value = 0.3, step = 0.05)
                 )
        ),
        
        # 能力・難易度タブ
        tabPanel("能力・難易度",
                 # 分布設定パネル
                 wellPanel(
                   h4("能力・難易度パラメータ"),
                   radioButtons("participant_distribution", "受験者能力の分布:",
                                choices = list(
                                  "正規分布" = "normal",
                                  "一様分布" = "uniform"
                                ),
                                selected = "normal"),
                   sliderInput("participant_mean", "受験者能力の平均:",
                               min = -6.0, max = 6.0, value = 0.0, step = 0.1),
                   sliderInput("participant_sd", "受験者能力のばらつき:",
                               min = 0.05, max = 6.0, value = 1.0, step = 0.1),
                   
                   hr(),
                   
                   sliderInput("rater_mean", "評価者の厳しさの平均:",
                               min = -6.0, max = 6.0, value = 0.0, step = 0.1),
                   sliderInput("rater_sd", "評価者の厳しさのばらつき:",
                               min = 0.05, max = 6.0, value = 0.2, step = 0.05),
                   sliderInput("rater_effect_size", "評価者効果の大きさ:",
                               min = 0.5, max = 6.0, value = 1.0, step = 0.1),
                   
                   hr(),
                   
                   sliderInput("task_mean", "タスク難易度の平均:",
                               min = -6.0, max = 6.0, value = 0.0, step = 0.1),
                   sliderInput("task_sd", "タスク難易度のばらつき:",
                               min = 0.05, max = 6.0, value = 0.2, step = 0.05),
                   sliderInput("task_effect_size", "タスク効果の大きさ:",
                               min = 0.5, max = 6.0, value = 1.0, step = 0.1),
                   
                   hr(),
                   
                   sliderInput("criteria_mean", "評価観点難易度の平均:",
                               min = -6.0, max = 6.0, value = 0.0, step = 0.1),
                   sliderInput("criteria_sd", "評価観点難易度のばらつき:",
                               min = 0.05, max = 6.0, value = 0.2, step = 0.05),
                   sliderInput("criteria_effect_size", "評価観点効果の大きさ:",
                               min = 0.5, max = 6.0, value = 1.0, step = 0.1)
                 )
        ),
        
        # 閾値設定タブ
        tabPanel("閾値設定",
                 wellPanel(
                   h4("閾値パラメータ設定"),
                   uiOutput("dynamic_thresholds"),
                   helpText("注意: 閾値は自動的に昇順に並べ替えられます")
                 )
        ),
        
        # プリセットタブ
        tabPanel("プリセット",
                 wellPanel(
                   h4("パラメータプリセット"),
                   actionButton("preset_high_uni", "強い一次元性", 
                                class = "btn-success", width = "100%"),
                   br(), br(),
                   actionButton("preset_medium_uni", "中程度の一次元性", 
                                class = "btn-info", width = "100%"),
                   br(), br(),
                   actionButton("preset_multi_dim", "明らかな多次元性", 
                                class = "btn-warning", width = "100%"),
                   br(), br(),
                   actionButton("preset_extreme_uni", "極端な一次元性", 
                                class = "btn-primary", width = "100%"),
                   hr(),
                   downloadButton("download_params", "現在のパラメータを保存"),
                   br(), br(),
                   fileInput("upload_params", "パラメータを読み込む")
                 )
        )
      ),
      
      # 実行ボタン
      actionButton("run_analysis", "分析実行", 
                   class = "btn-lg btn-primary", width = "100%")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        tabPanel("分析結果",
                 fluidRow(
                   column(6, 
                          h4("一次元性評価", align = "center"),
                          tableOutput("dim_table")
                   ),
                   column(6, 
                          h4("使用中のパラメータ", align = "center"),
                          tableOutput("param_table")
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(6, plotOutput("scree_plot", height = "400px")),
                   column(6, plotOutput("var_plot", height = "400px"))
                 ),
                 hr(),
                 fluidRow(
                   column(12, h4("スコア分布", align = "center")),
                   column(12, plotOutput("score_dist", height = "300px"))
                 )
        ),
        tabPanel("データ",
                 h4("生成されたMFRMデータ", align = "center"),
                 fluidRow(
                   column(12, 
                          wellPanel(
                            fluidRow(
                              column(4, downloadButton("download_csv", "CSV形式でダウンロード")),
                              column(4, downloadButton("download_excel", "Excel形式でダウンロード")),
                              column(4, downloadButton("download_rds", "RDS形式でダウンロード"))
                            )
                          )
                   )
                 ),
                 DTOutput("data_table")
        ),
        tabPanel("パラメータ詳細",
                 h4("現在のパラメータ設定一覧", align = "center"),
                 verbatimTextOutput("full_params")
        ),
        tabPanel("Fit統計量",
                 fluidRow(
                   column(12, h4("ファセット別Fit統計量", align = "center")),
                   column(12, 
                          tabsetPanel(
                            tabPanel("受験者", DTOutput("participant_fit_table")),
                            tabPanel("評価者", DTOutput("rater_fit_table")),
                            tabPanel("タスク", DTOutput("task_fit_table")),
                            tabPanel("評価観点", DTOutput("criteria_fit_table"))
                          )
                   )
                 ),
                 fluidRow(
                   column(12, 
                          wellPanel(
                            downloadButton("download_fit_stats", "Fit統計量をダウンロード", class = "btn-info")
                          )
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(12, h4("Pathway Maps", align = "center")),
                   column(6, plotOutput("participant_pathway", height = "400px")),
                   column(6, plotOutput("rater_pathway", height = "400px"))
                 ),
                 fluidRow(
                   column(6, plotOutput("task_pathway", height = "400px")),
                   column(6, plotOutput("criteria_pathway", height = "400px"))
                 )
        ),
        tabPanel("Wright Map",
                 fluidRow(
                   column(12, plotOutput("wright_map", height = "600px"))
                 )
        ),
        tabPanel("信頼性分析",
                 fluidRow(
                   column(12, h4("Separation・Reliability分析", align = "center")),
                   column(12, tableOutput("reliability_table"))
                 ),
                 hr(),
                 fluidRow(
                   column(12, plotOutput("reliability_plot", height = "400px"))
                 ),
                 hr(),
                 fluidRow(
                   column(12, h4("信頼性指標の解釈", align = "center")),
                   column(12, 
                          wellPanel(
                            h5("Separation（分離指標）", align = "center"),
                            p("ファセット内のパラメータの真の分散と推定誤差の比率から計算される分離指標です。"),
                            p("Separation = √(パラメータの真の分散 / 誤差分散)")
                          )
                   )
                 ),
                 fluidRow(
                   column(12, 
                          wellPanel(
                            h5("Strata（層別指標）", align = "center"),
                            p("パラメータを統計的に区別できる層の数を示します。"),
                            p("Strata = (4 × Separation + 1) / 3"),
                            p(strong("解釈:")),
                            p("値が大きいほど、ファセット内でより細かな層別が可能であることを示します。例えば、受験者能力においてStrata = 5の場合、受験者を5つの異なる能力層に有意に区別できることを意味します。")
                          )
                   )
                 ),
                 fluidRow(
                   column(12, 
                          wellPanel(
                            h5("Reliability（信頼性）", align = "center"),
                            p("ファセットのパラメータ推定の信頼性を示します。クロンバックのαに類似しています。"),
                            p("Reliability = パラメータの真の分散 / (パラメータの真の分散 + 誤差分散)")
                          )
                   )
                 ),
                 fluidRow(
                   column(12,
                          wellPanel(
                            h5("各ファセットの信頼性指標の意味", align = "center"),
                            tags$ul(
                              tags$li(strong("受験者の信頼性："), "受験者能力パラメータの推定精度を示します。高い信頼性は、受験者間の能力差が信頼性高く測定されていることを意味します。"),
                              tags$li(strong("評価者の信頼性："), "評価者の厳しさパラメータの推定精度を示します。高い信頼性は、評価者間の厳しさの違いが一貫して測定されていることを意味します。"),
                              tags$li(strong("タスクの信頼性："), "タスク難易度パラメータの推定精度を示します。高い信頼性は、タスク間の難易度の違いが明確に区別できることを意味します。"),
                              tags$li(strong("評価観点の信頼性："), "評価観点の難易度パラメータの推定精度を示します。高い信頼性は、評価観点間の難易度の違いが一貫して測定されていることを意味します。")
                            )
                          )
                   )
                 )
        ),
        tabPanel("カテゴリ確率曲線",
                 fluidRow(
                   column(12, plotOutput("category_curves", height = "400px"))
                 )
        ),
        tabPanel("残差診断",
                 fluidRow(
                   column(12, h4("残差相関分析", align = "center")),
                   column(12, plotOutput("residual_correlation", height = "600px"))
                 )
        ),
        # ここに新しいヘルプタブを追加
        tabPanel("ヘルプ",
                 fluidRow(
                   column(12, 
                          h3("多相ラッシュモデル（MFRM）ヘルプページ", align = "center"),
                          hr(),
                          tabsetPanel(
                            tabPanel("クロス設計",
                                     uiOutput("cross_design_help")
                            ),
                            tabPanel("一次元性分析",
                                     uiOutput("dimensionality_help")
                            ),
                            tabPanel("パラメータ設定",
                                     uiOutput("parameter_help")
                            ),
                            tabPanel("Fit統計量",
                                     uiOutput("fit_help")
                            ),
                            tabPanel("信頼性",
                                     uiOutput("reliability_help")
                            )
                          )
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # 閾値の初期値を保持する反応オブジェクト
  thresholds <- reactiveVal(c(-1.2, -0.4, 0.4, 1.2))
  
  # 閾値の動的UI
  output$dynamic_thresholds <- renderUI({
    if (input$auto_generate_thresholds) {
      # 自動生成モードでは表示のみ
      threshold_values <- generate_thresholds(input$rating_scale_min, input$rating_scale_max)
      thresholds(threshold_values)
      
      tagList(
        helpText("評価スケールに基づいて閾値が自動生成されています"),
        tableOutput("threshold_table")
      )
    } else {
      # 現在のスケールに合わせた閾値スライダーを生成
      n_thresholds <- input$rating_scale_max - input$rating_scale_min
      threshold_inputs <- lapply(1:n_thresholds, function(i) {
        threshold_value <- thresholds()[i]
        if (is.na(threshold_value)) threshold_value <- -3.0 + (i-1) * 6.0/n_thresholds
        
        sliderInput(
          inputId = paste0("threshold_", i),
          label = paste0("閾値", i, ":"),
          min = -3.0,
          max = 3.0,
          value = threshold_value,
          step = 0.1
        )
      })
      
      do.call(tagList, threshold_inputs)
    }
  })
  
  # 閾値テーブル表示
  output$threshold_table <- renderTable({
    threshold_values <- thresholds()
    data.frame(
      閾値番号 = 1:length(threshold_values),
      閾値値 = round(threshold_values, 2)
    )
  })
  
  # 閾値を更新
  observeEvent(input$update_thresholds, {
    if (input$auto_generate_thresholds) {
      # 自動生成モードの場合
      new_thresholds <- generate_thresholds(input$rating_scale_min, input$rating_scale_max)
      thresholds(new_thresholds)
    } else {
      # 手動設定モードの場合、現在の値を取得
      n_thresholds <- input$rating_scale_max - input$rating_scale_min
      new_thresholds <- numeric(n_thresholds)
      
      for (i in 1:n_thresholds) {
        threshold_id <- paste0("threshold_", i)
        if (!is.null(input[[threshold_id]])) {
          new_thresholds[i] <- input[[threshold_id]]
        } else {
          new_thresholds[i] <- -3.0 + (i-1) * 6.0/n_thresholds
        }
      }
      
      # 閾値を昇順にソート
      new_thresholds <- sort(new_thresholds)
      thresholds(new_thresholds)
    }
  })
  
  # スケール変更時に閾値を更新
  observeEvent(c(input$rating_scale_min, input$rating_scale_max), {
    if (input$auto_generate_thresholds) {
      new_thresholds <- generate_thresholds(input$rating_scale_min, input$rating_scale_max)
      thresholds(new_thresholds)
    }
  })
  
  # プリセット設定
  observeEvent(input$preset_high_uni, {
    updateSliderInput(session, "residual_sd", value = 0.2)
    updateSliderInput(session, "within_facet_corr", value = 0.1)
    updateSliderInput(session, "participant_sd", value = 1.5)
    updateSliderInput(session, "rater_sd", value = 0.1)
    updateSliderInput(session, "task_sd", value = 0.1)
    updateSliderInput(session, "criteria_sd", value = 0.1)
    updateRadioButtons(session, "residual_type", selected = 1)
    updateRadioButtons(session, "participant_distribution", selected = "normal")
    updateSliderInput(session, "participant_mean", value = 0)
    updateSliderInput(session, "rater_effect_size", value = 0.8)
    updateSliderInput(session, "task_effect_size", value = 0.8)
    updateSliderInput(session, "criteria_effect_size", value = 0.8)
  })
  
  observeEvent(input$preset_medium_uni, {
    updateSliderInput(session, "residual_sd", value = 0.4)
    updateSliderInput(session, "within_facet_corr", value = 0.2)
    updateSliderInput(session, "participant_sd", value = 1.0)
    updateSliderInput(session, "rater_sd", value = 0.2)
    updateSliderInput(session, "task_sd", value = 0.2)
    updateSliderInput(session, "criteria_sd", value = 0.2)
    updateRadioButtons(session, "residual_type", selected = 1)
    updateRadioButtons(session, "participant_distribution", selected = "normal")
    updateSliderInput(session, "participant_mean", value = 0)
    updateSliderInput(session, "rater_effect_size", value = 1.0)
    updateSliderInput(session, "task_effect_size", value = 1.0)
    updateSliderInput(session, "criteria_effect_size", value = 1.0)
  })
  
  observeEvent(input$preset_multi_dim, {
    updateSliderInput(session, "residual_sd", value = 0.7)
    updateSliderInput(session, "within_facet_corr", value = 0.6)
    updateSliderInput(session, "participant_sd", value = 0.8)
    updateSliderInput(session, "rater_sd", value = 0.4)
    updateSliderInput(session, "task_sd", value = 0.4)
    updateSliderInput(session, "criteria_sd", value = 0.4)
    updateRadioButtons(session, "residual_type", selected = 3)
    updateRadioButtons(session, "participant_distribution", selected = "bimodal")
    updateSliderInput(session, "rater_effect_size", value = 1.5)
    updateSliderInput(session, "task_effect_size", value = 1.5)
    updateSliderInput(session, "criteria_effect_size", value = 1.5)
  })
  
  observeEvent(input$preset_extreme_uni, {
    updateSliderInput(session, "residual_sd", value = 0.1)
    updateSliderInput(session, "within_facet_corr", value = 0.05)
    updateSliderInput(session, "participant_sd", value = 2.0)
    updateSliderInput(session, "rater_sd", value = 0.05)
    updateSliderInput(session, "task_sd", value = 0.05)
    updateSliderInput(session, "criteria_sd", value = 0.05)
    updateRadioButtons(session, "residual_type", selected = 1)
    updateRadioButtons(session, "participant_distribution", selected = "normal")
    updateSliderInput(session, "participant_mean", value = 0)
    updateSliderInput(session, "rater_effect_size", value = 0.5)
    updateSliderInput(session, "task_effect_size", value = 0.5)
    updateSliderInput(session, "criteria_effect_size", value = 0.5)
  })
  
  # 現在のパラメータを取得する関数
  get_current_params <- reactive({
    # 閾値を取得
    current_thresholds <- thresholds()
    
    list(
      n_participants = input$n_participants,
      n_raters = input$n_raters,
      n_tasks = input$n_tasks,
      n_criteria = input$n_criteria,
      residual_type = as.numeric(input$residual_type),
      residual_sd = input$residual_sd,
      within_facet_corr = input$within_facet_corr,
      participant_distribution = input$participant_distribution,
      participant_mean = input$participant_mean,
      participant_sd = input$participant_sd,
      rater_mean = input$rater_mean,
      rater_sd = input$rater_sd,
      task_mean = input$task_mean,
      task_sd = input$task_sd,
      criteria_mean = input$criteria_mean,
      criteria_sd = input$criteria_sd,
      rater_effect_size = input$rater_effect_size,
      task_effect_size = input$task_effect_size,
      criteria_effect_size = input$criteria_effect_size,
      threshold_values = current_thresholds,
      rating_scale_min = input$rating_scale_min,
      rating_scale_max = input$rating_scale_max,
      auto_generate_thresholds = input$auto_generate_thresholds,
      cross_design = input$cross_design,
      rater_assignment_rate = input$rater_assignment_rate,
      task_assignment_rate = input$task_assignment_rate,
      criteria_assignment_rate = input$criteria_assignment_rate,
      missing_data_rate = input$missing_data_rate,
      balanced_blocks = input$balanced_blocks,
      seed_value = input$seed_value
    )
  })
  
  # パラメータ保存
  output$download_params <- downloadHandler(
    filename = function() {
      paste("mfrm-params-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(get_current_params(), file)
    }
  )
  
  # パラメータ読み込み
  observeEvent(input$upload_params, {
    req(input$upload_params)
    params <- readRDS(input$upload_params$datapath)
    
    # 各パラメータを更新
    updateNumericInput(session, "n_participants", value = params$n_participants)
    updateNumericInput(session, "n_raters", value = params$n_raters)
    updateNumericInput(session, "n_tasks", value = params$n_tasks)
    updateNumericInput(session, "n_criteria", value = params$n_criteria)
    updateRadioButtons(session, "residual_type", selected = params$residual_type)
    updateSliderInput(session, "residual_sd", value = params$residual_sd)
    updateSliderInput(session, "within_facet_corr", value = params$within_facet_corr)
    updateRadioButtons(session, "participant_distribution", selected = params$participant_distribution)
    updateSliderInput(session, "participant_mean", value = params$participant_mean)
    updateSliderInput(session, "participant_sd", value = params$participant_sd)
    updateSliderInput(session, "rater_mean", value = params$rater_mean)
    updateSliderInput(session, "rater_sd", value = params$rater_sd)
    updateSliderInput(session, "task_mean", value = params$task_mean)
    updateSliderInput(session, "task_sd", value = params$task_sd)
    updateSliderInput(session, "criteria_mean", value = params$criteria_mean)
    updateSliderInput(session, "criteria_sd", value = params$criteria_sd)
    updateSliderInput(session, "rater_effect_size", value = params$rater_effect_size)
    updateSliderInput(session, "task_effect_size", value = params$task_effect_size)
    updateSliderInput(session, "criteria_effect_size", value = params$criteria_effect_size)
    updateNumericInput(session, "rating_scale_min", value = params$rating_scale_min)
    updateNumericInput(session, "rating_scale_max", value = params$rating_scale_max)
    
    # 閾値の更新
    if (!is.null(params$threshold_values)) {
      thresholds(params$threshold_values)
    }
    
    # 自動生成フラグの更新
    if (!is.null(params$auto_generate_thresholds)) {
      updateCheckboxInput(session, "auto_generate_thresholds", value = params$auto_generate_thresholds)
    }
  })
  
  # 反応オブジェクト
  results <- reactiveVal(NULL)
  
  # 分析実行
  observeEvent(input$run_analysis, {
    # プログレスバー表示
    withProgress(message = '分析実行中...', value = 0, {
      
      # 「閾値を自動生成」がチェックされている場合は最新のスケールに基づいて閾値を更新
      if (input$auto_generate_thresholds) {
        new_thresholds <- generate_thresholds(input$rating_scale_min, input$rating_scale_max)
        thresholds(new_thresholds)
      } else {
        # 手動設定の場合は既存の閾値数とスケールに不一致がないか確認
        n_thresholds_needed <- input$rating_scale_max - input$rating_scale_min
        if (length(thresholds()) != n_thresholds_needed) {
          # 不一致がある場合は閾値を再生成
          new_thresholds <- generate_thresholds(input$rating_scale_min, input$rating_scale_max)
          thresholds(new_thresholds)
          
          # オプション：ユーザーに通知
          showNotification(
            "評価スケールと閾値の数が一致しないため、閾値を再生成しました。",
            type = "warning",
            duration = 5
          )
        }
      }
      
      # データ生成
      incProgress(0.2, detail = "データを生成中")
      current_thresholds <- thresholds()
      
      data_result <- generate_mfrm_data(
        n_participants = input$n_participants,
        n_raters = input$n_raters,
        n_tasks = input$n_tasks,
        n_criteria = input$n_criteria,
        residual_type = as.numeric(input$residual_type),
        residual_sd = input$residual_sd,
        within_facet_corr = input$within_facet_corr,
        participant_distribution = input$participant_distribution,
        participant_mean = input$participant_mean,
        participant_sd = input$participant_sd,
        rater_mean = input$rater_mean,
        rater_sd = input$rater_sd,
        task_mean = input$task_mean,
        task_sd = input$task_sd,
        criteria_mean = input$criteria_mean,
        criteria_sd = input$criteria_sd,
        rater_effect_size = input$rater_effect_size,
        task_effect_size = input$task_effect_size,
        criteria_effect_size = input$criteria_effect_size,
        threshold_values = current_thresholds,
        rating_scale_min = input$rating_scale_min,
        rating_scale_max = input$rating_scale_max,
        cross_design = input$cross_design,
        rater_assignment_rate = input$rater_assignment_rate,
        task_assignment_rate = input$task_assignment_rate,
        criteria_assignment_rate = input$criteria_assignment_rate,
        missing_data_rate = input$missing_data_rate,
        balanced_blocks = input$balanced_blocks,
        seed_value = input$seed_value
      )
      
      speaking_data <- data_result$data
      
      # 一意のパラメータ値を抽出
      unique_abilities <- unique(data_result$data[, c("participant_id", "ability")])
      unique_abilities <- unique_abilities[order(unique_abilities$participant_id), ]
      theta <- unique_abilities$ability
      
      unique_severities <- unique(data_result$data[, c("rater_id", "severity")])
      alpha <- unique_severities$severity
      
      unique_task_difficulties <- unique(data_result$data[, c("task", "task_difficulty")])
      beta <- unique_task_difficulties$task_difficulty
      
      unique_criteria_difficulties <- unique(data_result$data[, c("criteria", "criteria_difficulty")])
      gamma <- unique_criteria_difficulties$criteria_difficulty
      
      rater_ids <- unique(data_result$data$rater_id)
      task_names <- unique(data_result$data$task)
      criteria_names <- unique(data_result$data$criteria)
      
      # ファセットパラメータをリストにまとめる
      facet_params <- list(
        theta = theta,
        alpha = alpha,
        beta = beta,
        gamma = gamma,
        rater_ids = rater_ids,
        task_names = task_names,
        criteria_names = criteria_names,
        threshold_values = current_thresholds
      )
      
      # PCA分析
      incProgress(0.3, detail = "PCA分析実行中")
      analysis_results <- run_pca_analysis(speaking_data)
      
      # 一次元性評価
      incProgress(0.1, detail = "一次元性を評価中")
      dim_info <- create_dimensionality_info(analysis_results$dimensionality)
      
      # Fit統計量計算
      incProgress(0.2, detail = "Fit統計量計算中")
      fit_stats <- calculate_fit_statistics(analysis_results$speaking_data)
      
      # 信頼性統計計算 - 事前に準備したfacet_paramsを使用
      incProgress(0.2, detail = "信頼性指標を計算中")
      reliability_stats <- calculate_reliability_stats(
        facet_params = facet_params,
        speaking_data = speaking_data
      )
      
      # 結果を保存
      results(list(
        data_result = data_result,
        analysis = analysis_results,
        dim_info = dim_info,
        params = get_current_params(),
        fit_stats = fit_stats,
        reliability_stats = reliability_stats,
        facet_params = facet_params
      ))
    })
  })
  
  observeEvent(input$update_thresholds, {
    if (input$auto_generate_thresholds) {
      # 自動生成モードの場合
      new_thresholds <- generate_thresholds(input$rating_scale_min, input$rating_scale_max)
      thresholds(new_thresholds)
      
      # 閾値が更新されたことを通知
      showNotification(
        "閾値を自動生成しました。",
        type = "message",
        duration = 3
      )
    } else {
      # 手動設定モードの場合、現在の値を取得
      n_thresholds <- input$rating_scale_max - input$rating_scale_min
      new_thresholds <- numeric(n_thresholds)
      
      for (i in 1:n_thresholds) {
        threshold_id <- paste0("threshold_", i)
        if (!is.null(input[[threshold_id]])) {
          new_thresholds[i] <- input[[threshold_id]]
        } else {
          new_thresholds[i] <- -3.0 + (i-1) * 6.0/n_thresholds
        }
      }
      
      # 閾値を昇順にソート
      new_thresholds <- sort(new_thresholds)
      thresholds(new_thresholds)
      
      # 閾値が更新されたことを通知
      showNotification(
        "閾値を手動で更新しました。",
        type = "message",
        duration = 3
      )
    }
  })
  
  # データ構造サマリーをUI表示する関数
  output$dataset_structure <- renderUI({
    req(results())
    
    structure_analysis <- analyze_dataset_structure(results()$analysis$speaking_data)
    
    # データセット情報
    dataset_info <- structure_analysis$dataset_info
    participant_summary <- structure_analysis$participant_summary
    rater_summary <- structure_analysis$rater_summary
    
    # 連結性の状態メッセージ
    connectivity_status <- if(dataset_info$connectivity$sufficient_connectivity) {
      tags$span(style = "color: green; font-weight: bold;", "適切（すべての受験者が評価され、すべての評価者が使用されています）")
    } else {
      tags$span(style = "color: red; font-weight: bold;", "不十分（一部の受験者または評価者がデータセットから切り離されています）")
    }
    
    # HTML出力の構築
    tagList(
      h4("データセット統計", style = "text-align: center;"),
      
      wellPanel(
        fluidRow(
          column(4, 
                 h5("データセット全体", style = "text-align: center;"),
                 tags$ul(
                   tags$li(sprintf("総評価数: %d", dataset_info$total_evaluations)),
                   tags$li(sprintf("疎密度: %.2f%%", dataset_info$sparsity * 100)),
                   tags$li(HTML(sprintf("連結性: %s", as.character(connectivity_status))))
                 )
          ),
          column(4, 
                 h5("受験者統計", style = "text-align: center;"),
                 tags$ul(
                   tags$li(sprintf("受験者数: %d", participant_summary$total_participants)),
                   tags$li(sprintf("平均評価数: %.1f (最小: %d, 最大: %d)", 
                                   participant_summary$mean_evaluations, 
                                   participant_summary$min_evaluations, 
                                   participant_summary$max_evaluations)),
                   tags$li(sprintf("平均評価者数: %.1f (最小: %d, 最大: %d)", 
                                   participant_summary$mean_raters, 
                                   participant_summary$min_raters, 
                                   participant_summary$max_raters))
                 )
          ),
          column(4, 
                 h5("評価者統計", style = "text-align: center;"),
                 tags$ul(
                   tags$li(sprintf("評価者数: %d", rater_summary$total_raters)),
                   tags$li(sprintf("平均評価数: %.1f (最小: %d, 最大: %d)", 
                                   rater_summary$mean_evaluations, 
                                   rater_summary$min_evaluations, 
                                   rater_summary$max_evaluations)),
                   tags$li(sprintf("平均受験者数: %.1f (最小: %d, 最大: %d)", 
                                   rater_summary$mean_participants, 
                                   rater_summary$min_participants, 
                                   rater_summary$max_participants))
                 )
          )
        )
      ),
      
      # 連結性についての詳細情報
      wellPanel(
        h5("連結性詳細", style = "text-align: center;"),
        fluidRow(
          column(6,
                 tags$ul(
                   tags$li(sprintf("受験者あたりの最小評価者数: %d", 
                                   dataset_info$connectivity$min_raters_per_participant)),
                   tags$li(sprintf("受験者あたりの最大評価者数: %d", 
                                   dataset_info$connectivity$max_raters_per_participant))
                 )
          ),
          column(6,
                 tags$ul(
                   tags$li(sprintf("評価者あたりの最小受験者数: %d", 
                                   dataset_info$connectivity$min_participants_per_rater)),
                   tags$li(sprintf("評価者あたりの最大受験者数: %d", 
                                   dataset_info$connectivity$max_participants_per_rater))
                 )
          )
        )
      ),
      
      # 構造の理解を助けるヒント
      wellPanel(
        h5("解釈のガイド", style = "text-align: center;"),
        tags$p("疎密度: 100%に近いほど、多くの可能な組み合わせがデータセットから欠けていることを示します。"),
        tags$p("連結性: すべての受験者と評価者がデータセットで適切につながっている必要があります。不十分な連結性は、一部のパラメータが推定できない可能性があります。"),
        tags$p("理想的には、各受験者が少なくとも2人の評価者から評価され、各評価者が複数の受験者を評価することが望ましいです。")
      )
    )
  })
  
  # データのダウンロード処理
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("mfrm-data-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      req(results())
      write.csv(results()$analysis$speaking_data, file, row.names = FALSE)
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("mfrm-data-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      req(results())
      # Excelファイル用のライブラリがロード済みか確認
      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop("Excelエクスポートには'writexl'パッケージが必要です。\nインストールするには: install.packages('writexl')")
      }
      writexl::write_xlsx(results()$analysis$speaking_data, file)
    }
  )
  
  output$download_rds <- downloadHandler(
    filename = function() {
      paste("mfrm-data-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".rds", sep = "")
    },
    content = function(file) {
      req(results())
      saveRDS(results()$data_result, file)
    }
  )
  
  # 一次元性評価テーブル
  output$dim_table <- renderTable({
    req(results())
    results()$dim_info$table_data
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # パラメータ設定テーブル
  output$param_table <- renderTable({
    req(results())
    
    # 残差構造名
    residual_type_name <- c("ランダム残差", "評価者関連残差", "タスク関連残差")[results()$params$residual_type]
    
    # 評価スケール
    scale_range <- paste(results()$params$rating_scale_min, "〜", results()$params$rating_scale_max)
    
    # 主要パラメータのみ表示
    param_data <- data.frame(
      Parameter = c(
        "残差構造タイプ",
        "残差の標準偏差",
        "ファセット内の相関",
        "受験者能力のばらつき",
        "受験者能力分布",
        "評価者効果の大きさ",
        "タスク効果の大きさ",
        "評価スケール",
        "閾値数"
      ),
      Value = c(
        residual_type_name,
        format(results()$params$residual_sd, nsmall = 2),
        format(results()$params$within_facet_corr, nsmall = 2),
        format(results()$params$participant_sd, nsmall = 2),
        results()$params$participant_distribution,
        format(results()$params$rater_effect_size, nsmall = 2),
        format(results()$params$task_effect_size, nsmall = 2),
        scale_range,
        length(results()$params$threshold_values)
      )
    )
    param_data
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # 全パラメータ表示
  output$full_params <- renderPrint({
    req(results())
    results()$params
  })
  
  # スクリープロット
  output$scree_plot <- renderPlot({
    req(results())
    create_scree_plot(
      results()$analysis$dimensionality$eigenvalues,
      title = "スクリープロット"
    )
  })
  
  # 分散説明率プロット
  output$var_plot <- renderPlot({
    req(results())
    create_var_explained_plot(
      results()$analysis$dimensionality$pca_results$Variance,
      title = "分散説明率"
    )
  })
  
  # スコア分布プロット
  output$score_dist <- renderPlot({
    req(results())
    
    scale_min <- results()$params$rating_scale_min
    scale_max <- results()$params$rating_scale_max
    
    ggplot(results()$analysis$speaking_data, aes(x = score)) +
      geom_bar(fill = "steelblue") +
      labs(
        title = "スコア分布",
        x = "スコア",
        y = "頻度"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14)
      ) +
      scale_x_continuous(breaks = scale_min:scale_max)
  })
  
  # データテーブル
  output$data_table <- renderDT({
    req(results())
    results()$analysis$speaking_data %>%
      select(participant_id, rater_id, task, criteria, expected_score, std_residual, score) %>%
      datatable(
        options = list(
          pageLength = 15,
          scrollX = TRUE
        )
      )
  })
  
  # Fit統計量テーブル
  output$participant_fit_table <- renderDT({
    req(results())
    results()$fit_stats$participant %>%
      mutate(
        infit = round(infit, 2),
        outfit = round(outfit, 2),
        infit_t = round(infit_t, 2),
        outfit_t = round(outfit_t, 2)
      ) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      ) %>%
      formatStyle(
        'infit_t',
        backgroundColor = styleInterval(c(-2, 2), c('blue', 'green', 'red'))
      ) %>%
      formatStyle(
        'outfit_t',
        backgroundColor = styleInterval(c(-2, 2), c('blue', 'green', 'red'))
      )
  })
  
  output$rater_fit_table <- renderDT({
    req(results())
    results()$fit_stats$rater %>%
      mutate(
        infit = round(infit, 2),
        outfit = round(outfit, 2),
        infit_t = round(infit_t, 2),
        outfit_t = round(outfit_t, 2)
      ) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      ) %>%
      formatStyle(
        'infit_t',
        backgroundColor = styleInterval(c(-2, 2), c('blue', 'green', 'red'))
      ) %>%
      formatStyle(
        'outfit_t',
        backgroundColor = styleInterval(c(-2, 2), c('blue', 'green', 'red'))
      )
  })
  
  output$task_fit_table <- renderDT({
    req(results())
    results()$fit_stats$task %>%
      mutate(
        infit = round(infit, 2),
        outfit = round(outfit, 2),
        infit_t = round(infit_t, 2),
        outfit_t = round(outfit_t, 2)
      ) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      ) %>%
      formatStyle(
        'infit_t',
        backgroundColor = styleInterval(c(-2, 2), c('blue', 'green', 'red'))
      ) %>%
      formatStyle(
        'outfit_t',
        backgroundColor = styleInterval(c(-2, 2), c('blue', 'green', 'red'))
      )
  })
  
  output$criteria_fit_table <- renderDT({
    req(results())
    results()$fit_stats$criteria %>%
      mutate(
        infit = round(infit, 2),
        outfit = round(outfit, 2),
        infit_t = round(infit_t, 2),
        outfit_t = round(outfit_t, 2)
      ) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      ) %>%
      formatStyle(
        'infit_t',
        backgroundColor = styleInterval(c(-2, 2), c('blue', 'green', 'red'))
      ) %>%
      formatStyle(
        'outfit_t',
        backgroundColor = styleInterval(c(-2, 2), c('blue', 'green', 'red'))
      )
  })
  
  output$download_fit_stats <- downloadHandler(
    filename = function() {
      paste("mfrm-fit-stats-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      req(results())
      
      # 必要なライブラリの確認
      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop("Excelエクスポートには'writexl'パッケージが必要です。\nインストールするには: install.packages('writexl')")
      }
      
      # 全てのfit統計量をリストとして格納
      fit_data_list <- list(
        "受験者" = results()$fit_stats$participant,
        "評価者" = results()$fit_stats$rater,
        "タスク" = results()$fit_stats$task,
        "評価観点" = results()$fit_stats$criteria
      )
      
      # Excelファイルとして保存
      writexl::write_xlsx(fit_data_list, file)
    }
  )
  
  # 信頼性テーブルの出力
  output$reliability_table <- renderTable({
    req(results())
    format_reliability_table(results()$reliability_stats)
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # 信頼性グラフの出力
  output$reliability_plot <- renderPlot({
    req(results())
    create_reliability_plot(results()$reliability_stats)
  })
  
  # Pathway Maps
  output$participant_pathway <- renderPlot({
    req(results())
    create_pathway_map(results()$fit_stats, "participant")
  })
  
  output$rater_pathway <- renderPlot({
    req(results())
    create_pathway_map(results()$fit_stats, "rater")
  })
  
  output$task_pathway <- renderPlot({
    req(results())
    create_pathway_map(results()$fit_stats, "task")
  })
  
  output$criteria_pathway <- renderPlot({
    req(results())
    create_pathway_map(results()$fit_stats, "criteria")
  })
  
  # Wright Map
  output$wright_map <- renderPlot({
    req(results())
    create_wright_map(
      results()$analysis$speaking_data,
      results()$facet_params
    )
  })
  
  # カテゴリ確率曲線
  output$category_curves <- renderPlot({
    req(results())
    create_category_curves(
      results()$facet_params$threshold_values,
      results()$params$rating_scale_min
    )
  })
  
  # 残差相関行列プロット
  output$residual_correlation <- renderPlot({
    req(results())
    create_correlation_plot(results()$analysis$speaking_data)
  })
  
  # データテーブル
  output$data_table <- renderDT({
    req(results())
    results()$analysis$speaking_data %>%
      select(participant_id, rater_id, task, criteria, expected_score, std_residual, score) %>%
      datatable(
        options = list(
          pageLength = 15,
          scrollX = TRUE
        )
      )
  })
  
  # クロス設計ヘルプコンテンツ
  output$cross_design_help <- renderUI({
    # マークダウンをHTMLに変換するためのライブラリが必要
    if (!requireNamespace("markdown", quietly = TRUE)) {
      return(HTML("<p>マークダウンの表示には 'markdown' パッケージが必要です。</p>
                <code>install.packages('markdown')</code>"))
    }
    
    # ヘルプ用のマークダウンテキスト
    help_md <- "# 多相ラッシュモデル（MFRM）におけるクロス設計ガイド

## クロス設計とは？

クロス設計とは、評価研究やテスト設計において、受験者・評価者・タスク・評価観点の組み合わせをどのように構成するかを決定する方法です。つまり、「どの受験者が、どの評価者から、どのタスクで、どの評価観点に基づいて評価されるか」という割り当てを指します。

## MFRMの4つの主要ファセット

MFRMには通常、以下の4つの主要なファセットが含まれます：

1. **受験者**：評価を受ける人（能力パラメータを持つ）
2. **評価者**：評価を行う人（厳しさパラメータを持つ）
3. **タスク**：実施される評価活動（難易度パラメータを持つ）
4. **評価観点**：評価の基準となる側面（難易度パラメータを持つ）

これら4つのファセットの組み合わせによって、様々な評価設計が可能になります。

## クロス設計の種類

このアプリケーションでは、以下の3種類のクロス設計から選択できます：

### 1. 完全クロス設計（Complete）

すべての受験者が、すべての評価者から、すべてのタスクと評価観点で評価される設計です。

**視覚的表現 - 受験者×評価者**：

```
         評価者A  評価者B  評価者C
受験者1    ✓       ✓       ✓
受験者2    ✓       ✓       ✓
受験者3    ✓       ✓       ✓
```

**視覚的表現 - 受験者×タスク**：

```
         タスク1  タスク2  タスク3
受験者1    ✓       ✓       ✓
受験者2    ✓       ✓       ✓
受験者3    ✓       ✓       ✓
```

**視覚的表現 - タスク×評価観点**：

```
         観点1   観点2   観点3   観点4
タスク1    ✓      ✓      ✓      ✓
タスク2    ✓      ✓      ✓      ✓
タスク3    ✓      ✓      ✓      ✓
```

**特徴**：
- すべての組み合わせが存在するため、データが最も豊富
- パラメータ推定の精度が高い
- 実施コストが非常に高い（特に受験者数や評価者数が多い場合）
- 実際の大規模テスト場面では実現が困難な場合が多い

**推奨される状況**：
- 少人数の詳細な研究
- パイロット調査
- 高い推定精度が必要な場合

### 2. バランス設計（Balanced）

受験者をグループに分け、各グループに評価者の一部を割り当てる設計です。同様に、各受験者に特定のタスクと評価観点が割り当てられます。グループ間のバランスを保ちながら、必要なデータの接続性を確保します。

**視覚的表現 - 受験者×評価者**：

```
         評価者A  評価者B  評価者C  評価者D
受験者1    ✓       ✓       ×       ×
受験者2    ✓       ✓       ×       ×
受験者3    ×       ×       ✓       ✓
受験者4    ×       ×       ✓       ✓
```

**視覚的表現 - 受験者×タスク（タスク割り当て率50%の場合）**：

```
         タスク1  タスク2  タスク3  タスク4
受験者1    ✓       ✓       ×       ×
受験者2    ×       ✓       ✓       ×
受験者3    ×       ×       ✓       ✓
受験者4    ✓       ×       ×       ✓
```

**特徴**：
- 完全クロス設計より効率的
- グループ間の接続性が確保される
- 評価者効果とタスク効果の比較が可能
- データの一部のみを収集するため、コスト削減

**推奨される状況**：
- 中〜大規模の評価研究
- リソースが限られている場合
- 評価者間・タスク間比較が重要な場合

### 3. ランダム設計（Random）

各受験者にランダムな評価者とタスク、評価観点を割り当てる設計です。

**視覚的表現 - 受験者×評価者**：

```
         評価者A  評価者B  評価者C  評価者D
受験者1    ✓       ×       ✓       ×
受験者2    ×       ✓       ×       ✓
受験者3    ✓       ×       ×       ✓
受験者4    ×       ✓       ✓       ×
```

**視覚的表現 - 受験者×タスク（ランダム割り当て）**：

```
         タスク1  タスク2  タスク3
受験者1    ✓       ×       ✓
受験者2    ×       ✓       ✓
受験者3    ✓       ✓       ×
受験者4    ✓       ×       ×
```

**特徴**：
- 柔軟性が高い
- データの接続性が偶然に依存する
- サンプルサイズが十分大きい場合に有効
- 分析時に接続性の確認が必要

**推奨される状況**：
- 大規模テスト
- 多数の受験者と評価者を扱う場合
- 完全に均等なバランスが不要な場合

## 関連するパラメータ

### 1. 各受験者に割り当てる評価者の割合

評価者全体のうち、各受験者に割り当てる評価者の割合です。例えば、評価者が4人いて割合が0.5の場合、各受験者は2人の評価者から評価を受けることになります。

### 2. 各受験者に割り当てるタスクの割合

全タスクのうち、各受験者が実施するタスクの割合です。例えば、タスクが3つあって割合が0.66の場合、各受験者は2つのタスクを実施することになります。

### 3. 各評価に使用する評価観点の割合

全評価観点のうち、各評価で使用される評価観点の割合です。例えば、評価観点が4つあって割合が0.5の場合、各評価では2つの評価観点が使用されます。

### 4. 均等なブロックを作成（バランス設計のみ）

有効にすると、受験者を均等なグループに分け、各グループに評価者を均等に割り当てます。これにより、データの接続性が向上します。

### 5. 欠測データの発生率

データ収集中に発生する可能性のある欠測値の割合です。現実的なシミュレーションのために使用します。実際の評価場面では、様々な理由で一部のデータが収集できないことがあります。

## 多相ラッシュモデルにおけるクロス設計の重要性

MFRMにおいて適切なクロス設計は、以下の理由で非常に重要です：

1. **パラメータ推定の精度**：十分なデータ接続性があれば、すべてのパラメータ（受験者能力、評価者厳しさ、タスク難易度、評価観点難易度）を同一の尺度上で正確に推定できます。

2. **効率的なデータ収集**：完全クロス設計は理想的ですが、現実的ではない場合が多いため、効率的なクロス設計によりコストを削減しながら必要な情報を収集できます。

3. **バイアスの制御**：適切なクロス設計により、特定の評価者や特定のタスクによるバイアスを相殺できます。

## クロス設計の選択ガイド

1. **完全クロス設計**：少人数の研究や、最大限の精度が必要な場合に選択してください。

2. **バランス設計**：中〜大規模の研究で、評価者間の比較が重要な場合に選択してください。リソースを節約しつつ、信頼性の高いデータを得ることができます。

3. **ランダム設計**：大規模な調査や、柔軟性が必要な場合に選択してください。ただし、データの接続性を確保するため、割り当て率を適切に設定する必要があります。

## データの接続性（Connectivity）について

どのクロス設計を選択する場合も、データの接続性は重要です。接続性とは、すべての受験者、評価者、タスク、評価観点がデータセット内で適切につながっているかどうかを示します。

- **良好な接続性**：すべてのパラメータを一つの共通尺度上で比較可能
- **不十分な接続性**：データセットが分断され、グループ間の比較ができない

理想的には：
- 各受験者が少なくとも2人の評価者から評価を受ける
- 各受験者が複数のタスクを実施する
- 各タスクが複数の評価観点で評価される
- 各評価者が複数の受験者を評価する

## 実践的なアドバイス

1. **初めての方**：まずは「完全クロス設計」で小規模なデータセットから始めることをお勧めします。

2. **現実的なシミュレーション**：実際の評価研究を模擬する場合は「バランス設計」を選択し、適切な割り当て率を設定してください。

3. **大規模テスト**：大人数のテストシミュレーションには「ランダム設計」が適していますが、接続性を確保するため割り当て率を0.3以上に設定することをお勧めします。

4. **データの品質確認**：シミュレーション後、「データ」タブでデータ構造を確認し、十分な接続性があるか確認してください。"
    
    # マークダウンをHTMLに変換して表示
    HTML(markdown::markdownToHTML(text = help_md, fragment.only = TRUE))
  })
  
  # 一次元性分析ヘルプコンテンツ
  output$dimensionality_help <- renderUI({
    help_md <- "# 一次元性分析のガイド

## 一次元性とは？

測定における一次元性は、評価項目（評価者×タスク×観点）が単一の潜在特性（能力）を測定しているかどうかを示します。MFRMにおいては、モデルが一次元的であることを仮定しています。

## 本アプリでの一次元性分析方法

このアプリでは、以下の手法で一次元性を評価します：

1. **残差の主成分分析（PCA）**：標準化残差に対して主成分分析を行い、複数の次元の存在を確認します

2. **固有値分析**：固有値が1を超える主成分の数は、データに存在する可能性のある次元数を示します

3. **分散説明率**：第1主成分の分散説明率が高いほど、データの一次元性が強いと判断できます

## 一次元性の判断基準

このアプリでは、以下の基準で一次元性を評価します：

- **強い一次元性**：
  - 第1主成分の分散説明率 ≥ 20%
  - 固有値>1の主成分数 ≤ 2

- **中程度の一次元性**：
  - 第1主成分の分散説明率 ≥ 15%
  - 固有値>1の主成分数 ≤ 3

- **弱い一次元性または多次元**：
  - 上記の基準を満たさない場合

## 視覚的診断ツール

### スクリープロット

固有値のグラフで、急激な低下の後に平坦化するポイントが、データ内の意味のある次元数を示します。

### 分散説明率プロット

各主成分が説明する分散の割合を示します。第1主成分が他の主成分よりも大幅に高い説明率を持つ場合、一次元性が強いと判断できます。

### 残差相関ヒートマップ

残差間の相関パターンを視覚化します。強い相関パターンが見られる場合、それは多次元性の証拠となります。

## パラメータが一次元性に与える影響

以下のパラメータは、生成されるデータの一次元性に影響します：

### 残差構造タイプ

- **ランダム残差**：最も一次元性が高くなります
- **評価者関連残差**：評価者内の項目間に相関があり、多次元性が高まります
- **タスク関連残差**：タスク内の項目間に相関があり、多次元性が高まります

### 残差の標準偏差

値が小さいほど一次元性が強くなります。大きいほど多次元性が増加します。

### ファセット内の相関

値が大きいほど多次元性が強くなります。0に近いほど一次元性が高まります。

### 能力・難易度のばらつき

受験者能力のばらつきが大きいほど、一次元性が検出しやすくなります。

## プリセット機能

アプリには以下のプリセットが用意されています：

- **強い一次元性**：一次元性の高いデータを生成します
- **中程度の一次元性**：現実的な一次元性を持つデータを生成します
- **明らかな多次元性**：多次元構造を持つデータを生成します
- **極端な一次元性**：ほぼ完全な一次元性を持つデータを生成します

これらのプリセットを使用して、異なる一次元性条件でのデータの挙動を確認できます。"
    
    HTML(markdown::markdownToHTML(text = help_md, fragment.only = TRUE))
  })
  
  # パラメータ設定ヘルプコンテンツ
  output$parameter_help <- renderUI({
    help_md <- "# パラメータ設定ガイド

## 基本設定

### 受験者数
シミュレーションに含める受験者（テスト受験者）の数です。多いほど安定したパラメータ推定が可能ですが、計算時間が増加します。

### 評価者数
評価を行う評価者の数です。評価者効果の分析には最低2人以上が必要です。

### タスク数
評価対象となるタスク（例：スピーキングやライティングの課題）の数です。

### 評価観点数
各タスクで評価される観点（例：流暢さ、正確さ、内容）の数です。

## 評価スケール設定

### 評価スケール最小値・最大値
評価に使用するスケールの範囲を設定します（例：1～5）。

### 閾値を自動生成
有効にすると、評価スケールに基づいて閾値パラメータが自動的に生成されます。無効にすると手動で閾値を設定できます。

## 残差構造

### 残差構造タイプ
- **ランダム残差**：各観測値に独立したランダムな誤差を追加します。一次元性が最も高くなります。
- **評価者関連残差**：同じ評価者内の項目に相関のある誤差を追加します。評価者関連の多次元性が発生します。
- **タスク関連残差**：同じタスク内の項目に相関のある誤差を追加します。タスク関連の多次元性が発生します。

### 残差の標準偏差
残差の大きさを制御します。小さいほどモデルの一次元性が高くなります。大きいほどデータのノイズが増加します。

### ファセット内の相関
残差構造内の相関の強さを制御します。大きいほど多次元性が強くなります。

## 能力・難易度パラメータ

### 受験者能力の分布
受験者の能力値の分布タイプを選択します。
- **正規分布**：能力が平均を中心に正規分布します（最も一般的）
- **一様分布**：能力が範囲内で均等に分布します

### 受験者能力の平均・ばらつき
受験者集団の能力レベルと、その分散を設定します。

### 評価者の厳しさの平均・ばらつき
評価者の厳しさの平均レベルと、評価者間のばらつきを設定します。

### 評価者効果の大きさ
評価者の厳しさがスコアに与える影響の大きさを調整します。大きいほど評価者間の差が顕著になります。

### タスク難易度の平均・ばらつき
タスクの難易度の平均レベルとタスク間のばらつきを設定します。

### タスク効果の大きさ
タスク難易度がスコアに与える影響の大きさを調整します。

### 評価観点難易度の平均・ばらつき
評価観点の難易度の平均レベルと観点間のばらつきを設定します。

### 評価観点効果の大きさ
評価観点の難易度がスコアに与える影響の大きさを調整します。

## 閾値パラメータ設定

カテゴリーの境界を表す閾値パラメータを設定します。例えば、5段階評価では4つの閾値があります。

## パラメータプリセット

### 強い一次元性
一次元性の高いデータを生成するための設定です。残差が小さく、ファセット内の相関が低い状態です。

### 中程度の一次元性
現実的な一次元性を持つデータを生成するための設定です。適度な残差とファセット内相関を持ちます。

### 明らかな多次元性
多次元構造を持つデータを生成するための設定です。残差が大きく、ファセット内の相関が高い状態です。

### 極端な一次元性
ほぼ完全な一次元性を持つデータを生成するための設定です。非常に小さな残差とほぼゼロのファセット内相関を持ちます。

## パラメータの保存と読み込み

現在のパラメータ設定を保存したり、以前保存した設定を読み込んだりすることができます。これにより、同じ条件で繰り返しシミュレーションを実行できます。"
    
    HTML(markdown::markdownToHTML(text = help_md, fragment.only = TRUE))
  })
  
  # Fit統計量ヘルプコンテンツ
  output$fit_help <- renderUI({
    help_md <- "# Fit統計量（適合度指標）のガイド

## Fit統計量とは？

Fit統計量は、データがモデルにどの程度適合しているかを示す指標です。MFRMでは、各受験者、評価者、タスク、評価観点ごとに適合度を計算し、適合・不適合を判定します。

## 主なFit統計量

### Infit（情報重み付き適合度）

モデルの期待から大きく外れた観測値に対して、より重みを与える統計量です。期待される分散が大きい場合（極端な能力値や難易度の場合）に対するモデル適合度を評価します。

### Outfit（外れ値敏感適合度）

単純な平均二乗標準化残差で、外れ値に敏感な統計量です。

### t統計量

Infit/Outfitの平方平均根（MNSQ）値を、より解釈しやすいt分布に変換した値です。通常、±2の範囲内であれば「適合」と判断します。

## 解釈の目安

### 適合（Acceptable Fit）

- **Infit/Outfit t統計量**: -2 ≤ t ≤ 2
- **意味**: データがモデルに適切に適合しています。

### 過適合（Overfit）

- **Infit/Outfit t統計量**: t < -2
- **意味**: データがモデルより予測可能（決定論的）すぎることを示します。実際のデータの変動がモデルの予測より小さいことを意味します。

### 不適合（Underfit）

- **Infit/Outfit t統計量**: t > 2
- **意味**: データがモデルから予測されるよりも大きく変動していることを示します。測定の一貫性を低下させる可能性があります。

## Pathway Maps

Pathway Mapは、各ファセット（受験者、評価者、タスク、評価観点）のInfit t統計量を視覚化したグラフです。

- **緑色の点**: 適合（-2 ≤ t ≤ 2）
- **青色の点**: 過適合（t < -2）
- **赤色の点**: 不適合（t > 2）

## Fit統計量の活用方法

### 受験者の適合度

不適合の受験者は、一貫性のない回答パターンを示している可能性があります。過適合の受験者は、過度に予測可能な回答パターン（例：同じカテゴリーの連続使用）を示している可能性があります。

### 評価者の適合度

不適合の評価者は、評価の一貫性が低く、信頼性の問題がある可能性があります。過適合の評価者は、評価基準を厳格に適用しすぎている可能性があります。

### タスクと評価観点の適合度

不適合のタスクや評価観点は、他の項目と異なる特性を測定している可能性があり、測定の一次元性を損なう可能性があります。

## Fit統計量と一次元性の関係

多数の不適合項目が存在する場合、データの多次元性が示唆されます。特に、特定のファセット（例：特定のタスクや評価観点）に不適合が集中している場合、それらの項目が別の次元を測定している可能性があります。

## 実践的なアドバイス

1. **適合度の分析**：まずは極端な不適合を示す項目（t > 3）を特定し、その原因を検討します。

2. **測定の改善**：不適合の項目を修正・除外することで、測定の質を向上させることができます。

3. **評価者トレーニング**：評価者の不適合が多い場合、評価基準の理解や適用に関するトレーニングが必要かもしれません。

4. **項目の再検討**：特定のタスクや評価観点が常に不適合を示す場合、それらの項目の内容や採点基準を再検討する必要があります。"
    
    HTML(markdown::markdownToHTML(text = help_md, fragment.only = TRUE))
  })
  
  output$reliability_help <- renderUI({
    help_md <- "# 信頼性指標（Reliability Statistics）ガイド

## 信頼性指標とは？

信頼性指標は、MFRMにおける各ファセット（受験者、評価者、タスク、評価観点）のパラメータ推定の精度と安定性を評価するための統計値です。これらの指標は、測定の質と結果の解釈に関する重要な情報を提供します。

## 主な信頼性指標

### Separation（分離指標）

Separationは、ファセット内のパラメータの真の分散と推定誤差の比率から計算される分離指標です。

- **計算式**: Separation = √(パラメータの真の分散 / 誤差分散)
- **解釈**:
  - Separation < 1.5: 分離が弱い（ファセット内のパラメータが有意に区別できない）
  - 1.5 < Separation < 3.0: 中程度の分離
  - Separation > 3.0: 良好な分離（ファセット内のパラメータが明確に区別できる）

### Strata（層別指標）

Strataは、パラメータを統計的に区別できる層の数を示します。

- **計算式**: Strata = (4 × Separation + 1) / 3
- **解釈**: 値が大きいほど、ファセット内でより細かな層別が可能であることを示します。例えば、受験者能力においてStrata = 5の場合、受験者を5つの異なる能力層に有意に区別できることを意味します。

### Reliability（信頼性）

Reliabilityは、ファセットのパラメータ推定の信頼性を示します。クロンバックのαに類似しています。

- **計算式**: Reliability = パラメータの真の分散 / (パラメータの真の分散 + 誤差分散)
- **解釈**:
  - Reliability < 0.7: 低い信頼性
  - 0.7 < Reliability < 0.9: 中程度の信頼性
  - Reliability > 0.9: 高い信頼性

## 各ファセットの信頼性指標の意味

### 受験者の信頼性

受験者能力パラメータの推定精度を示します。高い信頼性は、受験者間の能力差が信頼性高く測定されていることを意味します。

- **高いSeparation/Reliability**: 受験者の能力差が明確に識別できる
- **低いSeparation/Reliability**: 受験者の能力差が明確でない、または測定誤差が大きい

### 評価者の信頼性

評価者の厳しさパラメータの推定精度を示します。高い信頼性は、評価者間の厳しさの違いが一貫して測定されていることを意味します。

- **高いSeparation/Reliability**: 評価者間の厳しさの違いが明確に識別できる
- **低いSeparation/Reliability**: 評価者間の厳しさの違いが明確でない、または一貫性がない

### タスクの信頼性

タスク難易度パラメータの推定精度を示します。高い信頼性は、タスク間の難易度の違いが明確に区別できることを意味します。

- **高いSeparation/Reliability**: タスク間の難易度の違いが明確
- **低いSeparation/Reliability**: タスク間の難易度の違いが小さい、または一貫性がない

### 評価観点の信頼性

評価観点の難易度パラメータの推定精度を示します。高い信頼性は、評価観点間の難易度の違いが一貫して測定されていることを意味します。

- **高いSeparation/Reliability**: 評価観点間の難易度の違いが明確
- **低いSeparation/Reliability**: 評価観点間の難易度の違いが小さい、または一貫性がない

## 信頼性指標の向上方法

### 受験者の信頼性向上

- 評価項目数を増やす
- より弁別力のある項目を使用する
- 受験者のサンプルの能力範囲を広げる

### 評価者の信頼性向上

- 評価者の数を増やす
- 評価者トレーニングを強化する
- 各評価者が評価する受験者数を増やす

### タスクの信頼性向上

- タスクの難易度の範囲を広げる
- タスクの数を増やす
- より明確な評価基準を設定する

### 評価観点の信頼性向上

- 評価観点の定義を明確にする
- 評価観点の数を増やす
- 評価基準の詳細なルーブリックを作成する

## 信頼性指標の活用方法

1. **測定の質の評価**: 高い信頼性指標は測定の質が高いことを示します。

2. **テスト設計の改善**: 低い信頼性指標が見られる場合、テスト設計や評価方法の改善が必要かもしれません。

3. **結果の解釈**: 信頼性が低い場合、結果の解釈には慎重さが必要です。

4. **能力差の識別**: 受験者のSeparationとStrataは、能力の異なるグループを何段階に分けられるかを示します。これは、合格/不合格の判定や能力グループ分けに役立ちます。"
    
    HTML(markdown::markdownToHTML(text = help_md, fragment.only = TRUE))
  })
  
  # ヘルプタブのtabsetPanelに信頼性タブを追加
  tabPanel("信頼性指標",
           uiOutput("reliability_help")
  )
}

# アプリケーション実行
shinyApp(ui = ui, server = server)